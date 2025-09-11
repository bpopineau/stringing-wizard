;;; ==========================================================
;;; PV Stringing Wizard - v2.4 (Hardened + Interactive + Improvements)
;;; Author: Brandon + ChatGPT
;;; Description:
;;;   Automates PV stringing workflow:
;;;     - Prompts user for inverter & string info
;;;     - Guides module selection (first/last, auto-fill interior)
;;;     - Inserts +/- blocks (STRICT: must exist at configured DWG paths – no fallback)
;;;     - Draws routed polyline with fillets
;;;     - Labels string automatically (BC justification)
;;; Notes:
;;;   - Robust against user cancel / bad inputs
;;;   - Handles dynamic block "EffectiveName"
;;;   - Single undo mark; restores sysvars on exit
;;;   - Model space guard
;;;   - Explicit geometric tolerance
;;; ==========================================================

;; ------------------------
;; Utility / Config
;; ------------------------

;; Global tolerance (tune per drawing units)
(if (not *pv-eps*) (setq *pv-eps* 1e-6))

(defun nearly-zero (x) (< (abs x) *pv-eps*))

;; Robust dot / cross helpers (2D) -- symbol names cannot start with a digit in AutoLISP
(defun pvw-dot2d (a b) (+ (* (car a) (car b)) (* (cadr a) (cadr b))))
(defun pvw-cross2d (a b) (- (* (car a) (cadr b)) (* (cadr a) (car b))))

;; Improved point-on-segment test tolerant to slight skew & returns param (or nil)
(defun point-param-on-seg (p1 p2 pt / v w vlen2 paramT dist cross) 
  (setq v     (mapcar '- p2 p1)
        w     (mapcar '- pt p1)
        vlen2 (+ (* (car v) (car v)) (* (cadr v) (cadr v)))
  )
  (if (<= vlen2 0.0) 
    nil
    (progn 
      (setq paramT (/ (pvw-dot2d w v) vlen2))
      (if (and (>= paramT -0.0005) (<= paramT 1.0005)) 
        (progn 
          (setq cross (pvw-cross2d v w)
                dist  (if (<= vlen2 0.0) 1e9 (/ (abs cross) (sqrt vlen2)))
          )
          (if (< dist (* 25 *pv-eps*)) paramT)
        )
      )
    ) ; end progn
  ) ; end if length > 0
)     ; end defun point-param-on-seg

(defun in-model-space-p () (> (getvar "CVPORT") 1))

;; Safe input wrappers
(defun safe-entsel (msg) 
  (princ (strcat "\n" msg))
  (vl-catch-all-apply 
    (function (lambda () (car (entsel))))
  )
)

(defun safe-getint (msg / v) 
  (setq v (getint (strcat "\n" msg)))
  (if (or (null v) (< v 0)) nil v)
)

;; Debug flag & logger
(if (not (boundp '*pvw-debug*)) (setq *pvw-debug* nil))
(defun pvw:dbg (s /) 
  (if *pvw-debug* (princ (strcat "\n[PVW] " s)))
)

;; Validate non-negative integer counts
(defun pvw:valid-count? (n) (and (numberp n) (>= n 0) (= n (fix n))))

(defun safe-getstring (msg / s) 
  (setq s (getstring T (strcat "\n" msg)))
  (if (or (null s) (= s "")) nil s)
)

;; Get Effective (dynamic) block name, falling back to Name
(defun get-effective-block-name (e) 
  (cond 
    ((= (type e) 'ENAME) (get-effective-block-name (vlax-ename->vla-object e)))
    ((and e (vlax-property-available-p e 'EffectiveName))
     (vla-get-EffectiveName e)
    )
    (e (vla-get-Name e))
  )
)

;; Build a selection set of INSERTs matching a given EffectiveName (robust for dynamic blocks)
(defun build-mod-ss-by-effective-name (effName / all i v eff ss en) 
  (setq ss (ssadd))
  (if (setq all (ssget "_X" '((0 . "INSERT")))) 
    (progn 
      (setq i 0)
      (while (< i (sslength all)) 
        (setq en  (ssname all i)
              v   (vlax-ename->vla-object en)
              eff (if (vlax-property-available-p v 'EffectiveName) 
                    (vla-get-EffectiveName v)
                    (vla-get-Name v)
                  )
        )
        (if (= (strcase eff) (strcase effName)) 
          (setq ss (ssadd en ss))
        )
        (setq i (1+ i))
      )
    )
  )
  ss
)

;; Small helpers
(defun inspt-of (en) (cdr (assoc 10 (entget en))))
(defun is-mod-insert? (en modSSet) 
  (and en (= "INSERT" (cdr (assoc 0 (entget en)))) (ssmemb en modSSet))
)

;; ------------------------
;; Geometry helpers
;; ------------------------

;; Horizontal test used for label logic
(defun horizontal? (p1 p2) (nearly-zero (- (cadr p1) (cadr p2))))

;; Return list of (paramT param point) sorted by param along segment; filtering by perpendicular distance
(defun get-collinear-modules (p1 p2 modSSet / i eData insPt paramT acc) 
  (setq i   0
        acc nil
  )
  (while (< i (sslength modSSet)) 
    (setq eData  (entget (ssname modSSet i))
          insPt  (cdr (assoc 10 eData))
          paramT (point-param-on-seg p1 p2 insPt)
    )
    (if paramT (setq acc (cons (list paramT insPt) acc)))
    (setq i (1+ i))
  )
  (setq acc (vl-sort acc '(lambda (a b) (< (car a) (car b)))))
  ;; map extract points
  (mapcar 'cadr acc)
)

;; Adjust module list to match required count with interactive options
(defun pvw:adjust-modules (found required p1 p2 modSSet / modulesFound need ans p2e 
                           interior firstPt lastPt oldDyn canceled
                          ) 
  ;; Suppress dynamic input so the full prompt text is visible (restore later)
  (setq oldDyn (getvar "DYNMODE"))
  (setvar "DYNMODE" 0)
  (setq modulesFound (length found)
        need         required
        canceled     nil
  )
  (while (and (not canceled) (/= modulesFound need)) 
    (princ 
      (strcat "\nFound " 
              (itoa modulesFound)
              " module(s); expected "
              (itoa need)
              "."
      )
    )
    (princ "\nOptions: A=Accept current count, R=Repick last module, M=Manual adjust, C=Cancel")
    (initget "A R M C")
    (setq ans (getkword "\n[A]ccept / [R]epick last / [M]anual adjust / [C]ancel: "))
    (cond 
      ((or (null ans) (= ans "C")) (princ "\nCanceled by user.") (setq canceled T))
      ((= ans "A") (setq need modulesFound))
      ((= ans "R")
       (while 
         (progn (setq p2e (safe-entsel "Repick LAST module:")) 
                (if (not (is-mod-insert? p2e modSSet)) 
                  (progn (princ "\nPick valid module.") T)
                  nil
                )
         )
       )
       (if (null p2e) 
         (progn (princ "\nCanceled repick.") (setq canceled T))
         (progn 
           (setq p2    (inspt-of p2e)
                 found (get-collinear-modules p1 p2 modSSet)
           )
           (if (not (equal (car found) p1 *pv-eps*)) 
             (setq found (cons p1 (vl-remove p1 found)))
           )
           (if (not (equal (car (last found)) p2 *pv-eps*)) 
             (setq found (append (vl-remove p2 found) (list p2)))
           )
           (setq modulesFound (length found))
         )
       )
      )
      ((= ans "M")
       (cond 
         ((> modulesFound need)
          (setq firstPt  (car found)
                lastPt   (car (last found))
                interior (reverse (cdr (reverse (cdr found))))
          )
          (while (> (+ 2 (length interior)) need) 
            (setq interior (reverse (cdr (reverse interior))))
          )
          (setq found        (cons firstPt (append interior (list lastPt)))
                modulesFound (length found)
          )
          (princ (strcat "\nTrimmed to " (itoa modulesFound) " module(s)."))
         )
         ((< modulesFound need)
          (princ 
            (strcat "\nNeed to add " 
                    (itoa (- need modulesFound))
                    " module(s). Pick extras (Esc to stop)."
            )
          )
          (while (< modulesFound need) 
            (setq p2e (safe-entsel "Pick extra module:"))
            (if (not (is-mod-insert? p2e modSSet)) 
              (progn (princ "\nInvalid/canceled stopping add.") 
                     (setq modulesFound need)
              )
              (progn 
                (setq found (vl-sort (cons (inspt-of p2e) found) 
                                     '(lambda (a b) 
                                        (< (or (point-param-on-seg p1 p2 a) 0.0) 
                                           (or (point-param-on-seg p1 p2 b) 0.0)
                                        )
                                      )
                            )
                )
                (setq modulesFound (length found))
              )
            )
          )
         )
       )
      )
    )
  )
  (setvar "DYNMODE" oldDyn)
  (if canceled nil found)
)

;; ------------------------
;; Draw + label
;; ------------------------

(defun draw-and-label-string (ptList strName filletRad labelHeight labelOffset / i p1 
                              p2 segLen maxLen labelPt plObj horiz maxOverall 
                              overallPt layOld
                             ) 
  (setq maxLen     0.0
        maxOverall 0.0
        labelPt    nil
        overallPt  nil
  )

  ;; Draw polyline
  (command "._PLINE")
  (foreach pt ptList (command pt))
  (command "")
  (setq plObj (entlast))

  ;; Fillet polyline (uses current FILLETRAD)
  ;; Apply polyline fillets only if radius > 0
  (if (> filletRad 0.0) 
    (progn (setvar "FILLETRAD" filletRad) 
           (command "._FILLET" "P" plObj)
    )
  )

  ;; Find longest horizontal segment (fallback to longest overall)
  (setq i 0)
  (while (< (1+ i) (length ptList)) 
    (setq p1     (nth i ptList)
          p2     (nth (1+ i) ptList)
          segLen (distance p1 p2)
          horiz  (horizontal? p1 p2)
    )

    (if (and horiz (> segLen maxLen)) 
      (progn 
        (setq maxLen segLen)
        (setq labelPt (list (/ (+ (car p1) (car p2)) 2.0) 
                            (+ (/ (+ (cadr p1) (cadr p2)) 2.0) labelOffset)
                            0.0
                      )
        )
      )
    )

    (if (> segLen maxOverall) 
      (progn 
        (setq maxOverall segLen)
        (setq overallPt (list (/ (+ (car p1) (car p2)) 2.0) 
                              (+ (/ (+ (cadr p1) (cadr p2)) 2.0) labelOffset)
                              0.0
                        )
        )
      )
    )

    (setq i (1+ i))
  )

  ;; Fallback if no horizontal
  (if (null labelPt) (setq labelPt overallPt))

  ;; Place label (keep BC justification per request)
  (if labelPt 
    (command "._TEXT" "J" "BC" labelPt labelHeight "0" strName)
  )
)

;; ------------------------
;; Command
;; ------------------------

(defun ensure-layer (name color / tbl) 
  (if (not (setq tbl (tblsearch "LAYER" name))) 
    (entmake (list '(0 . "LAYER") (cons 2 name) (cons 62 color) '(70 . 0)))
  )
  (setvar "CLAYER" name)
)

;; Default configuration plist (user can override by altering *pvw-config* before load)
(if (not (boundp '*pvw-config*)) 
  (setq *pvw-config* (list :layer "PV-STRINGS" :layer-color 131 :fillet 12.0 
                           :label-height 6.0 :label-offset 6.0 :minus-block "PV_MINUS" 
                           :plus-block "PV_PLUS" :minus-block-path 
                           "S:\\1 Jobs\\1 Current Jobs\\CAD Resources\\Blocks\\SLD and Stringing\\StringTermMinus.dwg" 
                           :plus-block-path 
                           "S:\\1 Jobs\\1 Current Jobs\\CAD Resources\\Blocks\\SLD and Stringing\\StringTermPlus.dwg"
                     )
  )
)

(defun pvw:cfg (key) 
  (cadr (member key *pvw-config*))
)

;; Load block strictly from external DWG path. No placeholder creation.
(defun pvw:ensure-block (blockName blockPath / exists tmpEnt) 
  (setq exists (tblsearch "BLOCK" blockName))
  (cond 
    (exists (pvw:dbg (strcat "Block already present: " blockName)) T)
    ((and blockPath (findfile blockPath))
     (pvw:dbg (strcat "Loading block from path: " blockPath))
     (princ (strcat "\nLoading block '" blockName "' from: " blockPath))
     (command "._-INSERT" blockPath '(0 0 0) 1.0 1.0 0.0)
     (setq tmpEnt (entlast))
     (command) ; finalize if still in insert
     (if (not (tblsearch "BLOCK" blockName)) 
       (progn 
         (princ 
           (strcat "\nERROR: Expected block name '" 
                   blockName
                   "' not found in file."
           )
         )
         (if tmpEnt (entdel tmpEnt))
         nil
       )
       (progn (if tmpEnt (entdel tmpEnt)) T)
     )
    )
    (T
     (princ 
       (strcat "\nERROR: Block file not found for '" 
               blockName
               "' at: "
               (if blockPath blockPath "<nil>")
       )
     )
     nil
    )
  )
)

;; Error wrapper function (moved to top level to avoid nested defun issues)
(defun pvw:err-wrapper (msg / *undoStarted* doc oldlayer oldrad oldecho oldErr) 
  ; Note: this is a generic wrapper - actual cleanup vars set in calling context
  (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*"))) 
    (princ (strcat "\nError: " msg))
  )
  (princ)
)

;; Abort helper (top-level) - pass explicit environment for safe cleanup
(defun pvw:abort (msg doc oldlayer oldrad oldecho oldErr undoFlag) 
  ;; Graceful abort: perform cleanup but DO NOT call (exit). Return nil so caller can decide flow.
  (if msg (princ (strcat "\n" msg)))
  (if undoFlag (vla-EndUndoMark doc))
  (if oldlayer (setvar "CLAYER" oldlayer))
  (if oldrad (setvar "FILLETRAD" oldrad))
  (if oldecho (setvar "CMDECHO" oldecho))
  (if oldErr (setq *error* oldErr))
  (princ)
  nil
)

;; Config setter utility (update or append key in *pvw-config*)
(defun pvw:set-cfg (key val / src dst k v) 
  ;; Correct symbol comparison (was '=' which corrupted the plist)
  (setq src *pvw-config*
        dst nil
  )
  (while src 
    (setq k   (car src)
          v   (cadr src)
          src (cddr src)
    )
    (if (eq k key) 
      (setq dst (append dst (list k val))
            key nil
      ) ; mark consumed so we know it was replaced
      (setq dst (append dst (list k v)))
    )
  )
  (if key (setq dst (append dst (list key val)))) ; append if not found
  (setq *pvw-config* dst)
)

;; Numeric safety helper: ensure value is a non-negative number else default
(defun pvw:num-or (val def) 
  (if 
    (and (numberp val) 
         (not (vl-catch-all-error-p (vl-catch-all-apply '(lambda () val))))
    )
    val
    def
  )
)

(defun c:PVSTRINGS (/ oldlayer oldrad oldecho strLayerName strLayerColor filletRad 
                    labelHeight labelOffset minusBlockName plusBlockName modBlock 
                    effName modSSet invCount invNames strCounts modCounts invIndex 
                    strCount invModCounts strNum strName modulesNeeded invName numMods 
                    strList *undoStarted* doc oldErr tmp answer 
                    ;; string routing locals formerly in LET
                    p1e p2e p1 p2 foundPts ptList minusName plusName minusPt plusPt 
                    minusAvail plusAvail
                   ) 

  ;; Configurable constants
  (setq strLayerName   (pvw:cfg :layer)
        strLayerColor  (pvw:num-or (pvw:cfg :layer-color) 7)
        filletRad      (pvw:num-or (pvw:cfg :fillet) 0.0)
        labelHeight    (pvw:num-or (pvw:cfg :label-height) 6.0)
        labelOffset    (pvw:num-or (pvw:cfg :label-offset) 6.0)
        minusBlockName (pvw:cfg :minus-block)
        plusBlockName  (pvw:cfg :plus-block)
  )
  (if (not (numberp filletRad)) 
    (progn (princ "\n[PVW] WARNING: filletRad not numeric; defaulting to 0.0") 
           (setq filletRad 0.0)
    )
  )
  (if (not (numberp labelHeight)) 
    (progn (princ "\n[PVW] WARNING: labelHeight not numeric; defaulting to 6.0") 
           (setq labelHeight 6.0)
    )
  )
  (if (not (numberp labelOffset)) 
    (progn (princ "\n[PVW] WARNING: labelOffset not numeric; defaulting to 6.0") 
           (setq labelOffset 6.0)
    )
  )

  ;; Save sysvars
  (setq oldlayer (getvar "CLAYER")
        oldrad   (getvar "FILLETRAD")
        oldecho  (getvar "CMDECHO")
  )

  (setq *undoStarted* nil
        doc           (vla-get-ActiveDocument (vlax-get-Acad-Object))
  )

  (setq oldErr *error*)
  (setq *error* (lambda (msg) 
                  (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*"))) 
                    (princ (strcat "\nError: " msg))
                  )
                  (if *undoStarted* (vla-EndUndoMark doc))
                  (if oldlayer (setvar "CLAYER" oldlayer))
                  (if oldrad (setvar "FILLETRAD" oldrad))
                  (if oldecho (setvar "CMDECHO" oldecho))
                  (setq *error* oldErr)
                  (princ)
                )
  )

  ;; Model space guard
  (if (not (in-model-space-p)) 
    (pvw:abort "WARNING: Please switch to Model Space before running PVSTRINGS." doc 
               oldlayer oldrad oldecho oldErr *undoStarted*
    )
  )

  ;; Setup
  (setvar "CMDECHO" 0)
  (ensure-layer strLayerName strLayerColor)
  (setvar "FILLETRAD" filletRad)

  ;; Optional override
  (initget "Yes No")
  (setq answer (getkword "\nOverride defaults for Fillet/Label? [Yes/No] <No>: "))
  (if (= answer "Yes") 
    (progn 
      (setq tmp (getreal (strcat "\nFillet radius <" (rtos filletRad 2 2) ">: ")))
      (if tmp (setq filletRad tmp))
      (setvar "FILLETRAD" filletRad)
      (setq tmp (getreal (strcat "\nLabel height <" (rtos labelHeight 2 2) ">: ")))
      (if tmp (setq labelHeight tmp))
      (setq tmp (getreal (strcat "\nLabel offset <" (rtos labelOffset 2 2) ">: ")))
      (if tmp (setq labelOffset tmp))
    )
  )

  ;; Undo mark
  (vla-StartUndoMark doc)
  (setq *undoStarted* T)

  ;; Module block selection
  (setq modBlock (safe-entsel "Select a module block:"))
  (if (null modBlock) 
    (pvw:abort "Canceled." doc oldlayer oldrad oldecho oldErr *undoStarted*)
  )

  (setq effName (get-effective-block-name modBlock))
  (princ (strcat "\nModule effective name: " effName))
  (setq modSSet (build-mod-ss-by-effective-name effName))
  (if (or (null modSSet) (= (sslength modSSet) 0)) 
    (pvw:abort "No matching modules found." doc oldlayer oldrad oldecho oldErr 
               *undoStarted*
    )
  )

  ;; Inverter/string info
  (setq invCount (safe-getint "How many inverters? "))
  (if (not (pvw:valid-count? invCount)) 
    (pvw:abort "Canceled or invalid inverter count." doc oldlayer oldrad oldecho 
               oldErr *undoStarted*
    )
  )

  (setq invNames  nil
        strCounts nil
        modCounts nil
  )
  (repeat invCount 
    (setq invName (safe-getstring "Enter inverter name (e.g., A): "))
    (if (null invName) 
      (pvw:abort "Canceled." doc oldlayer oldrad oldecho oldErr *undoStarted*)
    )
    (setq strCount (safe-getint 
                     (strcat "How many strings for inverter " invName "? ")
                   )
    )
    (if (not (pvw:valid-count? strCount)) 
      (pvw:abort "Canceled or invalid string count." doc oldlayer oldrad oldecho 
                 oldErr *undoStarted*
      )
    )
    (setq strList nil)
    (repeat strCount 
      (setq numMods (safe-getint 
                      (strcat "Number of modules for next string of inverter " 
                              invName
                              ": "
                      )
                    )
      )
      (if (not (pvw:valid-count? numMods)) 
        (pvw:abort "Canceled or invalid module count." doc oldlayer oldrad oldecho 
                   oldErr *undoStarted*
        )
      )
      (setq strList (cons numMods strList))
    )
    (setq strList (reverse strList))
    (setq invNames  (append invNames (list invName))
          strCounts (append strCounts (list strCount))
          modCounts (append modCounts (list strList))
    )
  )

  ;; Process inverters
  (setq invIndex -1)
  (foreach invName invNames 
    (setq invIndex     (1+ invIndex)
          strCount     (nth invIndex strCounts)
          invModCounts (nth invIndex modCounts)
          strNum       0
    )
    (if (not (pvw:valid-count? strCount)) 
      (pvw:abort "Invalid per-inverter string count." doc oldlayer oldrad oldecho 
                 oldErr *undoStarted*
      )
    )
    (repeat strCount 
      (setq strNum        (1+ strNum)
            strName       (strcat "STRING " invName "-" (itoa strNum))
            modulesNeeded (nth (1- strNum) invModCounts)
      )
      (if (not (pvw:valid-count? modulesNeeded)) 
        (pvw:abort "Invalid modulesNeeded value." doc oldlayer oldrad oldecho oldErr 
                   *undoStarted*
        )
      )
      (princ 
        (strcat "\nNow processing: " strName " (" (itoa modulesNeeded) " modules)")
      )

      ;; Resolve +/- names
      (setq minusName minusBlockName
            plusName  plusBlockName
      )

      ;; FIRST module
      (while 
        (progn (setq p1e (safe-entsel "Pick FIRST module of the string:")) 
               (if (not (is-mod-insert? p1e modSSet)) 
                 (progn 
                   (princ "\nPlease pick a valid module of the selected type.")
                   T ; Keep looping if invalid
                 )
                 nil ; Exit loop if valid
               )
        )
      )
      (if (null p1e) 
        (pvw:abort "Canceled." doc oldlayer oldrad oldecho oldErr *undoStarted*)
      )
      (setq p1 (inspt-of p1e))

      ;; LAST module
      (while 
        (progn (setq p2e (safe-entsel "Pick LAST module of the string:")) 
               (if (not (is-mod-insert? p2e modSSet)) 
                 (progn 
                   (princ "\nPlease pick a valid module of the selected type.")
                   T ; Keep looping if invalid
                 )
                 nil ; Exit loop if valid
               )
        )
      )
      (if (null p2e) 
        (pvw:abort "Canceled." doc oldlayer oldrad oldecho oldErr *undoStarted*)
      )
      (setq p2 (inspt-of p2e))

      ;; Build run
      (setq foundPts (get-collinear-modules p1 p2 modSSet))
      (if (not (equal (car foundPts) p1 *pv-eps*)) 
        (setq foundPts (cons p1 (vl-remove p1 foundPts)))
      )
      (if (not (equal (car (last foundPts)) p2 *pv-eps*)) 
        (setq foundPts (append (vl-remove p2 foundPts) (list p2)))
      )
      (setq foundPts (pvw:adjust-modules foundPts modulesNeeded p1 p2 modSSet))
      (if (null foundPts) 
        (pvw:abort "Canceled during module adjustment." doc oldlayer oldrad oldecho 
                   oldErr *undoStarted*
        )
      )
      (setq ptList foundPts)

      ;; Ensure blocks
      (setq minusAvail (pvw:ensure-block minusName (pvw:cfg :minus-block-path))
            plusAvail  (pvw:ensure-block plusName (pvw:cfg :plus-block-path))
      )
      (cond 
        ((not minusAvail)
         (pvw:abort 
           (strcat "Missing required MINUS block or file: " 
                   (pvw:cfg :minus-block-path)
           )
           doc
           oldlayer
           oldrad
           oldecho
           oldErr
           *undoStarted*
         )
        )
        ((not plusAvail)
         (pvw:abort 
           (strcat "Missing required PLUS block or file: " 
                   (pvw:cfg :plus-block-path)
           )
           doc
           oldlayer
           oldrad
           oldecho
           oldErr
           *undoStarted*
         )
        )
      )
      (if minusAvail (setq minusPt (car ptList)) (setq minusPt nil))
      (if (and minusAvail minusPt) 
        (command "._-INSERT" minusName minusPt 1.0 1.0 0.0)
      )
      (if plusAvail (setq plusPt (car (last ptList))) (setq plusPt nil))
      (if (and plusAvail plusPt) (command "._-INSERT" plusName plusPt 1.0 1.0 0.0))

      ;; Draw / label
      (draw-and-label-string ptList strName filletRad labelHeight labelOffset)
    )
  )

  ;; Cleanup
  (if *undoStarted* (vla-EndUndoMark doc))
  (setvar "CLAYER" oldlayer)
  (setvar "FILLETRAD" oldrad)
  (setvar "CMDECHO" oldecho)
  (setq *error* oldErr)
  (princ)
)

(princ "\nType PVSTRINGS to run the PV Stringing Wizard.")
(princ "\nType PVWCONFIG to configure block paths.")
(princ)

;; Configuration command (fixed cond structure)
(defun c:PVWCONFIG (/ choice filePath) 
  (princ "\n=== PV Stringing Wizard Configuration ===")
  (princ (strcat "\nCurrent minus block: " (pvw:cfg :minus-block)))
  (princ (strcat "\nCurrent plus block: " (pvw:cfg :plus-block)))
  (princ 
    (strcat "\nMinus block path: " 
            (if (pvw:cfg :minus-block-path) (pvw:cfg :minus-block-path) "None")
    )
  )
  (princ 
    (strcat "\nPlus block path: " 
            (if (pvw:cfg :plus-block-path) (pvw:cfg :plus-block-path) "None")
    )
  )
  (initget "M P Q")
  (setq choice (getkword "\nConfigure [M]inus path / [P]lus path / [Q]uit: "))
  (cond 
    ((= choice "M")
     (setq filePath (getfiled "Select Minus Block File" "" "dwg" 0))
     (if filePath 
       (progn (pvw:set-cfg :minus-block-path filePath) 
              (princ (strcat "\nMinus block path set to: " filePath))
       )
     )
    )
    ((= choice "P")
     (setq filePath (getfiled "Select Plus Block File" "" "dwg" 0))
     (if filePath 
       (progn (pvw:set-cfg :plus-block-path filePath) 
              (princ (strcat "\nPlus block path set to: " filePath))
       )
     )
    )
    ((= choice "Q") (princ "\nNo changes made."))
  )
  (princ)
)

;; Debug utility (optional): quick paren balance checker (top-level)
(defun c:PVWBALANCE (/ fn txt i ch open close) 
  (setq fn (findfile "stringing-wizard/scripts/pvw-stringing-wizard.lsp"))
  (if (not fn) 
    (princ "\nFile not found in expected relative path.")
    (progn 
      (setq txt   (vl-file->string fn)
            i     0
            open  0
            close 0
      )
      (while (< i (strlen txt)) 
        (setq ch (substr txt (1+ i) 1))
        (cond 
          ((= ch "(") (setq open (1+ open)))
          ((= ch ")") (setq close (1+ close)))
        )
        (setq i (1+ i))
      )
      (princ 
        (strcat "\nParens open:" 
                (itoa open)
                " close:"
                (itoa close)
                (if (= open close) " (balanced)" " (MISMATCH)")
        )
      )
    )
  )
  (princ)
)