; ================================================================
; PV STRINGING WIZARD — AutoLISP Skeleton (v0.1)
; Goal: Semi-automated stringing assistant fully inside AutoCAD.
; Brandon: this is the scaffold — you can run it, see prompts,
; and we’ll fill in routing/counting details step by step.
; ================================================================

;;; ------------------------------
;;; USER-CONFIGURABLE SETTINGS
;;; ------------------------------
(setq *pvw-minus-block* "S:/1 Jobs/1 Current Jobs/CAD Resources/Blocks/SLD and Stringing/StringTermMinus.dwg") ; absolute path to minus marker block
(setq *pvw-plus-block* "S:/1 Jobs/1 Current Jobs/CAD Resources/Blocks/SLD and Stringing/StringTermPlus.dwg")  ; absolute path to plus marker block
(setq *pvw-text-style* "Standard")
(setq *pvw-text-height* 0.125)             ; 1/8"
(setq *pvw-fillet-radius* 12.0)            ; 12 inches (assumes drawing units = inches)
(setq *pvw-eps* 1e-6)                      ; small tolerance

;;; ------------------------------
;;; UTILITIES
;;; ------------------------------

(defun pvw:msg (s /) (princ (strcat "\n[PVW] " s)))

; Get entity name safely
(defun pvw:entsel* (msg / e p) 
  (while (and (not e) (setq p (entsel (strcat "\n" msg)))) 
    (setq e (car p))
  )
  e
)

; Get VLA-Object from ename
(defun pvw:vlobj (e) (vlax-ename->vla-object e))

; Effective block name (handles dynamic blocks). Falls back to (2 . name).
(defun pvw:block-effective-name (e / v o n) 
  ;; Safe effective name resolver. Returns NIL if not a block reference.
  (if (not e) 
    nil
    (progn 
      (setq o (vl-catch-all-apply 'pvw:vlobj (list e)))
      (cond 
        ((or (vl-catch-all-error-p o) (null o))
         ;; Not a valid VLA object
         nil
        )
        ((vlax-property-available-p o 'EffectiveName)
         (vlax-get o 'EffectiveName)
        )
        (t
         (setq v (entget e))
         (cdr (assoc 2 v))
        )
      )
    )
  )
)

; Get insertion point of a block reference (WCS)
(defun pvw:block-insp (e / d pt) 
  ;; Return insertion point of block reference in WCS.
  ;; Previous version erroneously called (trans .. (car d) 0) where (car d) was (-1 . <ename>)
  ;; causing: bad argument type: coordinate system specification.
  (if (and e (setq d (entget e))) 
    (progn 
      (setq pt (cdr (assoc 10 d)))
      ;; For INSERT entities the 10 group is already in WCS; ensure 3D list.
      (if (= (length pt) 2) (setq pt (list (car pt) (cadr pt) 0.0)))
      pt
    )
    nil
  )
)

; Geometric center (bounding-box midpoint) of block reference; fallback to insertion pt
(defun pvw:block-center (e / o min max cen bb) 
  (setq o (pvw:vlobj e))
  (if 
    (and o 
         (not 
           (vl-catch-all-error-p 
             (setq bb (vl-catch-all-apply 
                        '(lambda () (vla-getboundingbox o 'min 'max))
                      )
             )
           )
         )
    )
    (progn 
      (setq min (vlax-safearray->list min))
      (setq max (vlax-safearray->list max))
      (setq cen (list (/ (+ (car min) (car max)) 2.0) 
                      (/ (+ (cadr min) (cadr max)) 2.0)
                      0.0 ; 2D work: flatten Z
                )
      )
    )
  )
  (if (and (listp cen) (= (length cen) 3)) 
    cen
    (pvw:block-insp e)
  )
)

; Insert a block (name or DWG path) at point silently (uniform scale=1, rot=0)
(defun pvw:insert-block (blk pt / acad doc space vlaObj ename) 
  (vl-load-com)
  ;; Normalize point to 3D list (x y z). Reject invalid data.
  (cond 
    ((and (listp pt) (= (length pt) 2)) (setq pt (append pt '(0.0))))
    ((and (listp pt) (= (length pt) 3)) nil)
    (T
     (pvw:msg "Invalid point for insertion; skipping block insert.")
     (setq pt nil)
    )
  )
  (if (null pt) (return nil))
  (setq acad (vlax-get-Acad-Object))
  (setq doc (vla-get-ActiveDocument acad))
  (setq space (if (= (getvar "CVPORT") 1) 
                (vla-get-PaperSpace doc)
                (vla-get-ModelSpace doc)
              )
  )
  (setq vlaObj (vl-catch-all-apply 'vla-InsertBlock 
                                   (list space 
                                         (vlax-3d-point pt)
                                         blk
                                         1.0
                                         1.0
                                         1.0
                                         0.0
                                   )
               )
  )
  (if (vl-catch-all-error-p vlaObj) 
    (progn 
      (pvw:msg 
        (strcat "Direct insert failed for '" blk "'. Using command fallback.")
      )
      (vl-cmdf "_.-INSERT" blk pt 1.0 1.0 0.0)
      (setq ename (entlast))
    )
    (setq ename (vlax-vla-object->ename vlaObj))
  )
  ename
)

; Start a polyline at pt
(defun pvw:pline-start (pt) (command "_.PLINE" pt))

; Continue polyline to pt
(defun pvw:pline-next (pt) (command pt))

; End polyline
(defun pvw:pline-end () (command ""))

; Fillet all polyline corners (simple pass) — skeleton
(defun pvw:fillet-polyline (plename rad /) 
  (command "_.FILLET" "_R" rad)
  ; naive corner-walk: user selection then repeat FILLET w/ radius across vertices is non-trivial.
  ; For now: apply fillet radius and let user pick two segments repeatedly would be manual.
  ; TODO(v1.1): Walk vertices, issue FILLET between segment pairs programmatically.
)

; Find the longest horizontal segment in a pline (ename) — returns midpoint
(defun pvw:pline-longest-horizontal-mid (plename / obj n i p1 p2 dx dy best bestmid 
                                         len
                                        ) 
  (setq obj (pvw:vlobj plename))
  (if (and obj (vlax-method-applicable-p obj 'GetPointAtParam)) 
    (progn 
      (setq n (fix (vlax-curve-getEndParam obj)))
      (setq i       0
            best    -1
            bestmid nil
      )
      (while (< i n) 
        (setq p1 (vlax-curve-getPointAtParam obj i)
              p2 (vlax-curve-getPointAtParam obj (+ i 1))
              dx (- (car p2) (car p1))
              dy (- (cadr p2) (cadr p1))
        )
        (if (< (abs dy) *pvw-eps*) 
          (progn 
            (setq len (abs dx))
            (if (> len best) 
              (progn 
                (setq best len)
                (setq bestmid (list (/ (+ (car p1) (car p2)) 2.0) 
                                    (/ (+ (cadr p1) (cadr p2)) 2.0)
                                    0.0
                              )
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )
  bestmid
)

; Place mtext at point with bottom-center justification
(defun pvw:place-label (pt txt /) 
  (command "_.MTEXT" "_J" "_BC" pt "_H" *pvw-text-height* "_S" *pvw-text-style* txt 
           ""
  )
)

; Fence-select inserts of a given block between two points (segment) — rough counter
(defun pvw:count-modules-on-segment (blkname p1 p2 / ss n) 
  ; Using a fence is a coarse but effective way to count crossings.
  ; NOTE: This counts any INSERT along the fence; filter by (2 . blkname).
  (setq ss (ssget "_F" (list p1 p2) (list '(0 . "INSERT") (cons 2 blkname))))
  (setq n (if ss (sslength ss) 0))
  n
)

; Build list of strings from user input
; Returns a flat list of plists: ( (inverter "A" name "STRING A-1" target 15) ... )
;; Split a comma-separated list into clean tokens
(defun pvw:split-csv (s / lst pos next token) 
  (setq lst '())
  (if (and s (> (strlen s) 0)) 
    (progn 
      (setq pos 0)
      (while (setq next (vl-string-search "," s pos)) 
        (setq token (substr s (+ 1 pos) (- next pos)))
        (setq token (vl-string-trim " \t" token))
        (if (> (strlen token) 0) (setq lst (append lst (list token))))
        (setq pos (+ next 1))
      )
      (setq token (substr s (+ 1 pos)))
      (setq token (vl-string-trim " \t" token))
      (if (> (strlen token) 0) (setq lst (append lst (list token))))
    )
  )
  lst
)

(defun pvw:collect-strings-info (/ ninv invnames tokens invlist final i nm nstr j 
                                 sname modules
                                ) 
  (setq ninv (getint "\nHow many inverters? "))
  (if (not ninv) (setq ninv 1))
  (setq invnames (getstring T 
                            "\nNames of inverters? (comma-separated, e.g., A,B): "
                 )
  )
  (setq tokens (pvw:split-csv invnames))
  (if (or (null tokens) (= (length tokens) 0)) 
    (progn 
      (setq invlist '())
      (foreach c (vl-string->list (substr "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 1 ninv)) 
        (setq invlist (append invlist (list (chr c))))
      )
    )
    (setq invlist tokens)
  )
  ;; Adjust list length to ninv (trim or extend with next letters)
  (while (> (length invlist) ninv) 
    (setq invlist (reverse (cdr (reverse invlist))))
  )
  (while (< (length invlist) ninv) 
    (setq invlist (append invlist (list (chr (+ 65 (length invlist))))))
  )
  (setq final '())
  (setq i 0)
  (while (< i ninv) 
    (setq nm (nth i invlist))
    (setq nstr (getint (strcat "\nNumber of strings for inverter " nm "? ")))
    (if (not nstr) (setq nstr 1))
    (setq j 1)
    (while (<= j nstr) 
      (setq sname (strcat "STRING " nm "-" (itoa j)))
      (setq modules (getint (strcat "\nNumber of modules for " sname "? ")))
      (if (not modules) (setq modules 1))
      ;; Store record as association list: ((inverter . "A") (name . "STRING A-1") (target . 15))
      (setq final (append final 
                          (list 
                            (list (cons 'inverter nm) 
                                  (cons 'name sname)
                                  (cons 'target modules)
                            )
                          )
                  )
      )
      (setq j (1+ j))
    )
    (setq i (1+ i))
  )
  final
)

; Ask user to pick a module of selected block-type; returns ename
(defun pvw:pick-module (blkname / e ok bname) 
  (setq ok nil)
  (while (not ok) 
    (setq e (pvw:entsel* (strcat "Select module for " blkname " (ESC to cancel)")))
    (cond 
      ((null e) (setq ok T) (setq e nil)) ; user canceled
      ((/= (cdr (assoc 0 (entget e))) "INSERT")
       (pvw:msg "Selection is not a block reference. Try again.")
      )
      (t
       (setq bname (pvw:block-effective-name e))
       (cond 
         ((null bname)
          (pvw:msg "Unable to read block name. Try again.")
         )
         ((= (strcase bname) (strcase blkname)) (setq ok T))
         (T
          (pvw:msg 
            (strcat "That is '" bname "', expected '" blkname "'. Try again.")
          )
         )
       )
      )
    )
  )
  e
)

; Get the quadrant connection point on the start circle, aimed toward next point — TODO placeholder
(defun pvw:pick-quadrant-point (center next / dx dy) 
  ; For now, just return the center. In v1.1 we’ll offset by marker radius in the quadrant.
  (setq dx (- (car next) (car center)))
  (setq dy (- (cadr next) (cadr center)))
  center
)

;;; ------------------------------
;;; MAIN — PVW STRING WIZARD
;;; ------------------------------

(defun c:PVW (/ modRef modPick blkName strings todo s startEnt startCtr left rem 
              plEnt segEndEnt segEndCtr segStartPt hit count added done lastCtr
             ) 
  (vl-load-com)
  (pvw:msg "Select ONE module block to identify the array type.")
  (setq modRef nil)
  (while (not modRef) 
    (setq modPick (pvw:entsel* "Select a module block (ESC to cancel)"))
    (cond 
      ((null modPick) (pvw:msg "Canceled.") (princ) (exit))
      ((/= (cdr (assoc 0 (entget modPick))) "INSERT")
       (pvw:msg "That is not a block reference. Try again.")
      )
      (t (setq modRef modPick))
    )
  )
  (setq blkName (pvw:block-effective-name modRef))
  (if (null blkName) 
    (progn (pvw:msg "Could not determine block name. Aborting.") (princ) (exit))
  )
  (pvw:msg (strcat "Module type: " blkName))

  (pvw:msg "Collecting inverter/string definitions…")
  (setq strings (pvw:collect-strings-info))
  (if (not strings) (progn (pvw:msg "No strings defined. Exiting.") (princ)))

  (pvw:msg "Starting stringing loop…")
  (setq todo strings)

  (foreach s todo 
    (setq rem (cdr (assoc 'target s)))
    (pvw:msg 
      (strcat "Route " (cdr (assoc 'name s)) " — target modules: " (itoa rem))
    )

    ; 1) Pick start module and process only if user selected one
    (setq startEnt (pvw:pick-module blkName))
    (if startEnt 
      (progn 
        (setq startCtr (pvw:block-center startEnt))
        (pvw:insert-block *pvw-minus-block* startCtr)
        (pvw:pline-start startCtr)
        (setq lastCtr startCtr)
        (setq added 1)
        (setq rem (max 0 (1- rem)))
        (setq done nil)
        (setq plEnt nil)
        (while (not done) 
          (if (<= rem 0) 
            (progn 
              (pvw:pline-next lastCtr) ; close at last point even if no final move
              (pvw:pline-end)
              (setq plEnt (entlast)) ; capture polyline before inserting plus block
              (pvw:insert-block *pvw-plus-block* lastCtr)
              (setq done T)
            )
            (progn 
              (pvw:msg 
                (strcat (itoa rem) 
                        " more modules needed for "
                        (cdr (assoc 'name s))
                        ". Select the next module in line…"
                )
              )
              (setq segEndEnt (pvw:pick-module blkName))
              (if (not segEndEnt) 
                (progn (pvw:msg "Canceled during segment selection.") 
                       (setq done T)
                )
                (progn 
                  (setq segEndCtr (pvw:block-center segEndEnt))
                  (setq segStartPt (pvw:pick-quadrant-point lastCtr segEndCtr))
                  (pvw:pline-next segEndCtr)
                  (setq hit (pvw:count-modules-on-segment 
                              blkName
                              segStartPt
                              segEndCtr
                            )
                  )
                  (setq count (max 1 hit))
                  (setq rem (max 0 (- rem count)))
                  (setq lastCtr segEndCtr)
                )
              )
            )
          )
        ) ; while
        (if (not plEnt) (setq plEnt (entlast))) ; safety fallback
        (pvw:fillet-polyline plEnt *pvw-fillet-radius*)
        (setq left (pvw:pline-longest-horizontal-mid plEnt))
        (if left 
          (pvw:place-label 
            (list (car left) (+ (cadr left) *pvw-text-height*) 0.0)
            (cdr (assoc 'name s))
          )
          (pvw:place-label lastCtr (cdr (assoc 'name s)))
        )
      )
      (pvw:msg "Skipping this string (no start module chosen).")
    )
  )

  (pvw:msg 
    "All strings processed. Review geometry, tweak fillets if needed. v0.1 skeleton complete."
  )
  (princ)
)

; ------------------------------------------------
; END — c:PVW
; ------------------------------------------------
(princ "\nType PVW to launch the PV Stringing Wizard (skeleton).")
(princ)
