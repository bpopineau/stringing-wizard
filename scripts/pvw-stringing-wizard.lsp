;;; ==========================================================
;;; PV Stringing Wizard - v2.3 (Hardened + Interactive)
;;; Author: Brandon + ChatGPT
;;; Description:
;;;   Automates PV stringing workflow:
;;;     - Prompts user for inverter & string info
;;;     - Guides module selection (first/last, auto-fill interior)
;;;     - Inserts +/- blocks (configurable; placeholders)
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

(defun collinear? (p1 p2 pt / v1 v2 cross) 
  (setq v1    (mapcar '- p2 p1)
        v2    (mapcar '- pt p1)
        cross (- (* (car v1) (cadr v2)) (* (cadr v1) (car v2)))
  )
  (nearly-zero cross)
)

(defun horizontal? (p1 p2) 
  (nearly-zero (- (cadr p1) (cadr p2)))
)

(defun within-bounds? (p1 p2 pt) 
  (and (<= (min (car p1) (car p2)) (car pt) (max (car p1) (car p2))) 
       (<= (min (cadr p1) (cadr p2)) (cadr pt) (max (cadr p1) (cadr p2)))
  )
)

(defun sort-points-along-line (p1 p2 pts) 
  (vl-sort pts (function (lambda (a b) (< (distance p1 a) (distance p1 b)))))
)

(defun get-collinear-modules (p1 p2 modSSet / result i eData insPt) 
  (setq result nil
        i      0
  )
  (while (< i (sslength modSSet)) 
    (setq eData (entget (ssname modSSet i))
          insPt (cdr (assoc 10 eData))
    )
    (if (and (collinear? p1 p2 insPt) (within-bounds? p1 p2 insPt)) 
      (setq result (cons insPt result))
    )
    (setq i (1+ i))
  )
  (sort-points-along-line p1 p2 result)
)

;; ------------------------
;; Draw + label
;; ------------------------

(defun draw-and-label-string (ptList strName filletRad labelHeight labelOffset / i p1 
                              p2 segLen maxLen labelPt plObj horiz maxOverall 
                              overallPt
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
  (command "._FILLET" "P" plObj)

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

(defun c:PVSTRINGS (/ oldlayer oldrad oldecho strLayerName strLayerColor filletRad 
                    labelHeight labelOffset minusBlockName plusBlockName modBlock 
                    effName modSSet invCount invNames strCounts modCounts invIndex 
                    strCount invModCounts strNum strName modulesNeeded invName numMods 
                    strList *undoStarted* doc
                   ) 

  ;; Configurable constants
  (setq strLayerName   "PV-STRINGS"
        strLayerColor  131
        filletRad      12.0
        labelHeight    6.0
        labelOffset    6.0 ;; Configurable +/- block names (placeholders)
        minusBlockName "PV_MINUS"
        plusBlockName  "PV_PLUS"
  )

  ;; Save sysvars
  (setq oldlayer (getvar "CLAYER")
        oldrad   (getvar "FILLETRAD")
        oldecho  (getvar "CMDECHO")
  )

  (setq *undoStarted* nil
        doc           (vla-get-ActiveDocument (vlax-get-Acad-Object))
  )

  (unwind-protect 
    (progn 
      ;; Model space guard
      (if (not (in-model-space-p)) 
        (progn 
          (princ "\n⚠ Please switch to Model Space before running PVSTRINGS.")
          (exit)
        )
      )

      ;; Setup
      (setvar "CMDECHO" 0)
      (if (tblsearch "LAYER" strLayerName) 
        (command "._LAYER" "S" strLayerName "")
        (command "._LAYER" "M" strLayerName "C" strLayerColor strLayerName "")
      )
      (setvar "FILLETRAD" filletRad)

      ;; Single undo mark (ensure we can end it in cleanup)
      (vla-StartUndoMark doc)
      (setq *undoStarted* T)

      ;; Ask user for module block (any instance)
      (setq modBlock (safe-entsel "Select a module block:"))
      (if (null modBlock) (progn (princ "\nCanceled.") (exit)))

      ;; Resolve effective name (handles dynamic blocks)
      (setq effName (get-effective-block-name modBlock))
      (princ (strcat "\nModule effective name: " effName))

      ;; Cache all modules matching effective name
      (setq modSSet (build-mod-ss-by-effective-name effName))
      (if (or (null modSSet) (= (sslength modSSet) 0)) 
        (progn (princ "\nNo matching modules found.") (exit))
      )

      ;; Collect inverter/string info
      (setq invCount (safe-getint "How many inverters? "))
      (if (null invCount) (progn (princ "\nCanceled or invalid.") (exit)))

      (setq invNames  nil
            strCounts nil
            modCounts nil
      )

      (repeat invCount 
        (setq invName (safe-getstring "Enter inverter name (e.g., A): "))
        (if (null invName) (progn (princ "\nCanceled.") (exit)))

        (setq strCount (safe-getint 
                         (strcat "How many strings for inverter " invName "? ")
                       )
        )
        (if (null strCount) (progn (princ "\nCanceled or invalid.") (exit)))

        (setq strList nil)
        (repeat strCount 
          (setq numMods (safe-getint 
                          (strcat "Number of modules for next string of inverter " 
                                  invName
                                  ": "
                          )
                        )
          )
          (if (null numMods) (progn (princ "\nCanceled or invalid.") (exit)))
          ;; use cons for speed, reverse later
          (setq strList (cons numMods strList))
        )
        (setq strList (reverse strList))
        (setq invNames (append invNames (list invName)))
        (setq strCounts (append strCounts (list strCount)))
        (setq modCounts (append modCounts (list strList)))
      )

      ;; ------------------------
      ;; Loop through inverters/strings
      ;; ------------------------
      (setq invIndex -1)
      (foreach invName invNames 
        (setq invIndex     (1+ invIndex)
              strCount     (nth invIndex strCounts)
              invModCounts (nth invIndex modCounts)
              strNum       0
        )

        (repeat strCount 
          (setq strNum        (1+ strNum)
                strName       (strcat "STRING " invName "-" (itoa strNum))
                modulesNeeded (nth (1- strNum) invModCounts)
          )

          (princ 
            (strcat "\nNow processing: " 
                    strName
                    " ("
                    (itoa modulesNeeded)
                    " modules)"
            )
          )

          ;; ------------------------
          ;; IMPLEMENTED TODO SECTION (interactive routing)
          ;; ------------------------
          (let 
            (p1e p2e p1 p2 foundPts ptList ans modulesFound need delta minusName 
                 plusName minusPt plusPt
            )

            ;; Resolve configurable +/- block names (with defaults if not bound)
            (setq minusName (if (and (boundp 'minusBlockName) minusBlockName) 
                              minusBlockName
                              "PV_MINUS"
                            )
            )
            (setq plusName (if (and (boundp 'plusBlockName) plusBlockName) 
                             plusBlockName
                             "PV_PLUS"
                           )
            )

            ;; Pick FIRST module
            (while 
              (progn 
                (setq p1e (safe-entsel "Pick FIRST module of the string:"))
                (if (not (is-mod-insert? p1e modSSet)) 
                  (progn (princ "\nPlease pick a valid module of the selected type.") 
                         T
                  )
                  nil
                )
              )
            )
            (if (null p1e) (progn (princ "\nCanceled.") (exit)))
            (setq p1 (inspt-of p1e))

            ;; Pick LAST module
            (while 
              (progn 
                (setq p2e (safe-entsel "Pick LAST module of the string:"))
                (if (not (is-mod-insert? p2e modSSet)) 
                  (progn (princ "\nPlease pick a valid module of the selected type.") 
                         T
                  )
                  nil
                )
              )
            )
            (if (null p2e) (progn (princ "\nCanceled.") (exit)))
            (setq p2 (inspt-of p2e))

            ;; Build ordered run along the line (including endpoints)
            (setq foundPts (get-collinear-modules p1 p2 modSSet))
            ;; Ensure endpoints present and at ends
            (if (not (equal (car foundPts) p1 *pv-eps*)) 
              (setq foundPts (cons p1 (vl-remove p1 foundPts)))
            )
            (if (not (equal (car (last foundPts)) p2 *pv-eps*)) 
              (setq foundPts (append (vl-remove p2 foundPts) (list p2)))
            )

            (setq modulesFound (length foundPts)
                  need         modulesNeeded
                  delta        (- modulesFound need)
            )

            ;; Handle mismatches
            (while (/= modulesFound need) 
              (princ 
                (strcat "\nFound " 
                        (itoa modulesFound)
                        " module(s); expected "
                        (itoa need)
                        "."
                )
              )
              (initget "A R M C") ; Accept / Repick last / Manual adjust / Cancel
              (setq ans (getkword "\n[A]ccept / [R]epick last / [M]anual adjust / [C]ancel: "))

              (cond 
                ;; Cancel
                ((or (null ans) (= ans "C")) (princ "\nCanceled.") (exit))

                ;; Accept as-is
                ((= ans "A")
                 (setq need modulesFound)
                )

                ;; Repick LAST endpoint and rebuild
                ((= ans "R")
                 (while 
                   (progn 
                     (setq p2e (safe-entsel "Repick LAST module:"))
                     (if (not (is-mod-insert? p2e modSSet)) 
                       (progn (princ "\nPlease pick a valid module of the selected type.") 
                              T
                       )
                       nil
                     )
                   )
                 )
                 (if (null p2e) (progn (princ "\nCanceled.") (exit)))
                 (setq p2 (inspt-of p2e))
                 (setq foundPts (get-collinear-modules p1 p2 modSSet))
                 (if (not (equal (car foundPts) p1 *pv-eps*)) 
                   (setq foundPts (cons p1 (vl-remove p1 foundPts)))
                 )
                 (if (not (equal (car (last foundPts)) p2 *pv-eps*)) 
                   (setq foundPts (append (vl-remove p2 foundPts) (list p2)))
                 )
                 (setq modulesFound (length foundPts)
                       delta        (- modulesFound need)
                 )
                )

                ;; Manual adjust: either trim interior or add extra points
                ((= ans "M")
                 (cond 
                   ;; Trim interior points from the end until count matches (preserve endpoints)
                   ((> modulesFound need)
                    (let* 
                      ((firstPt (car foundPts)) 
                        (lastPt (car (last foundPts)))
                        ;; interior = foundPts without first/last
                        (interior (reverse (cdr (reverse (cdr foundPts)))))
                      )
                      (while (> (+ 2 (length interior)) need) 
                        ;; drop one from the end of interior
                        (setq interior (reverse (cdr (reverse interior))))
                      )
                      (setq foundPts (cons firstPt (append interior (list lastPt))))
                      (setq modulesFound (length foundPts))
                      (princ 
                        (strcat "\nTrimmed to " (itoa modulesFound) " module(s).")
                      )
                    )
                   )
                   ;; Add more modules by picking them; keep run sorted
                   ((< modulesFound need)
                    (princ 
                      (strcat "\nNeed to add " 
                              (itoa (- need modulesFound))
                              " module(s). "
                              "Pick extra modules on the same line (Esc to stop)."
                      )
                    )
                    (while (< modulesFound need) 
                      (setq p2e (safe-entsel "Pick an extra module to include:"))
                      (if (not (is-mod-insert? p2e modSSet)) 
                        (progn (princ "\nInvalid pick or canceled; stopping manual add.") 
                               (setq modulesFound need)
                        ) ; break
                        (progn 
                          (setq foundPts (sort-points-along-line 
                                           p1
                                           p2
                                           (cons (inspt-of p2e) foundPts)
                                         )
                          )
                          (setq modulesFound (length foundPts))
                        )
                      )
                    )
                   )
                 )
                )
              )
            )

            ;; Final list of points to route
            (setq ptList foundPts)

            ;; Insert +/- placeholders if blocks exist (silent skip if missing)
            (if (and minusName (tblsearch "BLOCK" minusName)) 
              (progn (setq minusPt (car ptList)) 
                     (if minusPt 
                       (command "._-INSERT" minusName minusPt 1.0 1.0 0.0)
                     )
              )
            )
            (if (and plusName (tblsearch "BLOCK" plusName)) 
              (progn (setq plusPt (car (last ptList))) 
                     (if plusPt (command "._-INSERT" plusName plusPt 1.0 1.0 0.0))
              )
            )

            ;; Draw + label
            (draw-and-label-string ptList strName filletRad labelHeight labelOffset)
          )
          ;; ------------------------
          ;; END IMPLEMENTED TODO
          ;; ------------------------
        )
      )
    )

    ;; Cleanup (always)
    (progn 
      (if *undoStarted* (vla-EndUndoMark doc))
      (setvar "CLAYER" oldlayer)
      (setvar "FILLETRAD" oldrad)
      (setvar "CMDECHO" oldecho)
    )
  )
  (princ)
)

(princ "\nType PVSTRINGS to run the PV Stringing Wizard.")
(princ)
