; ================================================================
; PV STRINGING WIZARD — AutoLISP Skeleton (v0.1)
; Goal: Semi-automated stringing assistant fully inside AutoCAD.
; Brandon: this is the scaffold — you can run it, see prompts,
; and we’ll fill in routing/counting details step by step.
; ================================================================

;;; ------------------------------
;;; USER-CONFIGURABLE SETTINGS
;;; ------------------------------
(setq *pvw-minus-block* "PV_STRING_MINUS") ; name of "minus" marker block
(setq *pvw-plus-block* "PV_STRING_PLUS")  ; name of "plus"  marker block
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
  (setq o (pvw:vlobj e))
  (cond 
    ((and o (vlax-property-available-p o 'EffectiveName))
     (setq n (vlax-get o 'EffectiveName))
    )
    (t
     (setq v (entget e))
     (setq n (cdr (assoc 2 v)))
    )
  )
)

; Get insertion point of a block reference (WCS)
(defun pvw:block-insp (e / d) 
  (setq d (entget e))
  (trans (cdr (assoc 10 d)) (car d) 0)
)

; Insert a block at point (uniform scale=1, rot=0) — skeleton
(defun pvw:insert-block (blk pt /) 
  (command "_.-INSERT" blk "_S" 1.0 "_R" 0.0 pt "")
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
(defun pvw:pline-longest-horizontal-mid (plename / vla obj n i p1 p2 dx dy best 
                                         bestmid bestlen
                                        ) 
  (setq obj (pvw:vlobj plename))
  (setq n (fix (vlax-curve-getEndParam obj)))
  (setq i       0
        best    -1
        bestmid nil
  )
  (while (< i n) 
    (setq p1 (vlax-curve-getPointAtParam obj i))
    (setq p2 (vlax-curve-getPointAtParam obj (+ i 1)))
    (setq dx (- (car p2) (car p1)))
    (setq dy (- (cadr p2) (cadr p1)))
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
(defun pvw:collect-strings-info (/ ninv invnames invlist final i nm nstr s j sname 
                                 modules lst
                                ) 
  (setq ninv (getint "\nHow many inverters? "))
  (if (not ninv) (setq ninv 1))
  (setq invnames (getstring T 
                            "\nNames of inverters? (comma-separated, e.g., A,B): "
                 )
  )
  (if (or (not invnames) (= invnames "")) 
    (setq invlist (mapcar 'chr 
                          (vl-string->list 
                            (substr "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 1 ninv)
                          )
                  )
    )
    (setq invlist (vl-remove-if '(lambda (x) (= x "")) 
                                (vl-string->list 
                                  (vl-string-subst "" 
                                                   " "
                                                   (vl-string-translate "," 
                                                                        " "
                                                                        invnames
                                                   )
                                  )
                                )
                  )
    )
  )
  ; normalize names to list of strings
  (setq invlist (mapcar 
                  '(lambda (c) 
                     (if (listp c) (vl-prin1-to-string c) (vl-prin1-to-string c))
                   )
                  invlist
                )
  )
  (setq final '())
  (setq i 0)
  (repeat ninv 
    (setq nm (nth i invlist))
    (setq nstr (getint (strcat "\nNumber of strings for inverter " nm "? ")))
    (if (not nstr) (setq nstr 1))
    (setq j 1)
    (repeat nstr 
      (setq sname (strcat "STRING " nm "-" (itoa j)))
      (setq modules (getint (strcat "\nNumber of modules for " sname "? ")))
      (if (not modules) (setq modules 1))
      (setq final (append final 
                          (list (list 'inverter nm 'name sname 'target modules))
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
    (setq e (pvw:entsel* (strcat "Select module for " blkname)))
    (cond 
      ((null e) (setq ok T)) ; user canceled
      (T
       (setq bname (pvw:block-effective-name e))
       (if (= (strcase bname) (strcase blkname)) 
         (setq ok T)
         (pvw:msg 
           (strcat "That is '" bname "', expected '" blkname "'. Try again.")
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

(defun c:PVW (/ modRef blkName strings todo s startEnt startCtr left rem plEnt 
              segEndEnt segEndCtr segStartPt hit count added done lastCtr
             ) 
  (vl-load-com)
  (pvw:msg "Select ONE module block to identify the array type.")
  (setq modRef (pvw:entsel* "Select a module block"))
  (if (not modRef) (progn (pvw:msg "Canceled.") (princ)))
  (setq blkName (pvw:block-effective-name modRef))
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

    ; 1) Pick start module and place MINUS block
    (setq startEnt (pvw:pick-module blkName))
    (if (not startEnt) (progn (pvw:msg "User canceled.") (exit)))
    (setq startCtr (pvw:block-insp startEnt))
    (pvw:insert-block *pvw-minus-block* startCtr)

    ; 2) Begin polyline from proper quadrant (placeholder: center)
    (pvw:pline-start startCtr)
    (setq lastCtr startCtr)
    (setq added 1) ; we count the start module as included
    (setq rem (max 0 (1- rem)))

    ; 3) While modules remaining, ask user for next module on this leg
    (setq done nil)
    (while (not done) 
      (if (<= rem 0) 
        (progn 
          ; Place PLUS at the lastCtr and terminate the pline there
          (pvw:pline-next lastCtr)
          (pvw:pline-end)
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
            (progn (pvw:msg "Canceled during segment selection.") (setq done T))
            (progn 
              (setq segEndCtr (pvw:block-insp segEndEnt))
              ; Compute start connection point (quadrant from minus/last) — placeholder
              (setq segStartPt (pvw:pick-quadrant-point lastCtr segEndCtr))

              ; Draw segment
              (pvw:pline-next segEndCtr)

              ; Count modules along this segment (coarse fence count)
              (setq hit (pvw:count-modules-on-segment blkName segStartPt segEndCtr))

              ; Deduct: we counted the end module as well, so hits includes last module? Fence typically counts anything along the fence; adjust roughly:
              ; Conservative approach: ensure at least 1 (the end). Tweak later with a bbox/center-line hit test.
              (setq count (max 1 hit))

              (setq rem (max 0 (- rem count)))
              (setq lastCtr segEndCtr)
            )
          )
        )
      )
    ) ; while

    ; 4) Post-process this polyline: fillet corners 12"
    ;    (We need its ename. In this skeleton we used command PLINE, so grab last created via entlast.)
    (setq plEnt (entlast))
    (pvw:fillet-polyline plEnt *pvw-fillet-radius*)

    ; 5) Label: find longest horizontal segment mid and place text
    (setq left (pvw:pline-longest-horizontal-mid plEnt))
    (if left 
      (pvw:place-label 
        (list (car left) (+ (cadr left) *pvw-text-height*) 0.0)
        (cdr (assoc 'name s))
      )
      (progn 
        ; fallback: label at geometric middle of pline
        (pvw:place-label lastCtr (cdr (assoc 'name s)))
      )
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
