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
;;; CHANGELOG (recent)
;;;   v2.5 (internal refactor draft)
;;;     - Added plan summary + confirmation before geometry
;;;     - Extracted configuration & balance utilities out of main command
;;;     - Added pvw:print-plan, pvw:confirm helpers
;;;     - Removed duplicated trailing / orphan code after main command
;;;     - Improved configuration command with more adjustable runtime values
;;;     - Style: more consistent prompts & user messaging
;;;
;;; TODO (future ideas)
;;;   - Add DCL dialog for plan entry (tabular inverter/string editing)
;;;   - Support saving/loading JSON/CSV plan templates
;;;   - Optional automatic path routing (orthogonal snapping / avoidance)
;;;   - Highlight candidate modules during first/last picks
;;;   - Multi-segment string runs with midpoints (user adds bends)
;;;   - Option to place label at user-specified point when summary displayed
;;;   - Undo per-string optionally (current single undo mark is all-or-nothing)
;;;   - Add QA function to revalidate all strings after edits
;;; ==========================================================

;; ------------------------
;; Utility / Config
;; ------------------------
;; ==========================================================
;; PV Stringing Wizard (Simplified Core)
;; Version: 3.0-slim
;; Purpose: ONLY pick a module block and report insertion + geometric center.
;; Author: Brandon + ChatGPT (refactored slim version)
;; ==========================================================
;; To extend: build on the reported points list and add routing separately.
;; ==========================================================

(defun in-model-space-p () (> (getvar "CVPORT") 1))
(defun safe-entsel (msg) 
  (princ (strcat "\n" msg))
  (vl-catch-all-apply '(lambda () (car (entsel))))
)
(defun get-effective-block-name (e) 
  (cond 
    ((= (type e) 'ENAME) (get-effective-block-name (vlax-ename->vla-object e)))
    ((and e (vlax-property-available-p e 'EffectiveName))
     (vla-get-EffectiveName e)
    )
    (e (vla-get-Name e))
  )
)
(defun inspt-of (en) (cdr (assoc 10 (entget en))))
(defun ensure-layer (name color / tbl) 
  (if (not (setq tbl (tblsearch "LAYER" name))) 
    (entmake (list '(0 . "LAYER") (cons 2 name) (cons 62 color) '(70 . 0)))
  )
  (setvar "CLAYER" name)
)
;; Basic config plist (kept minimal)
(if (not (boundp '*pvw-config*)) 
  (setq *pvw-config* (list :layer "PV-STRINGS" :layer-color 131))
)
(defun pvw:cfg (key) (cadr (member key *pvw-config*)))
;; Block center helper
(defun pvw:block-center (en / vla minPt maxPt _min _max) 
  (if (= (type en) 'ENAME) (setq vla (vlax-ename->vla-object en)) (setq vla en))
  (if (not vla) 
    nil
    (progn (vl-catch-all-apply '(lambda () (vla-getboundingbox vla 'minPt 'maxPt))) 
           (if (and minPt maxPt) 
             (progn 
               (setq _min (vlax-safearray->list minPt)
                     _max (vlax-safearray->list maxPt)
               )
               (list (/ (+ (car _min) (car _max)) 2.0) 
                     (/ (+ (cadr _min) (cadr _max)) 2.0)
                     (/ (+ (caddr _min) (caddr _max)) 2.0)
               )
             )
           )
    )
  )
)

;; Configuration command (limited for slim mode)
(defun c:PVWCONFIG (/ choice tmp) 
  (princ "\n=== PVW Slim Config ===")
  (princ (strcat "\nLayer: " (pvw:cfg :layer)))
  (princ (strcat "\nLayer Color: " (itoa (pvw:cfg :layer-color))))
  (initget "Layer Color Quit")
  (setq choice (getkword "\nChange [Layer/Color/Quit] <Quit>: "))
  (cond 
    ((= choice "Layer")
     (setq tmp (getstring T "\nNew layer name: "))
     (if (/= tmp "") (setq *pvw-config* (subst tmp (pvw:cfg :layer) *pvw-config*)))
    )
    ((= choice "Color")
     (setq tmp (getint "\nNew color number: "))
     (if tmp (setq *pvw-config* (subst tmp (pvw:cfg :layer-color) *pvw-config*)))
    )
    (T (princ "\nNo changes."))
  )
  (princ)
)

;; Main simplified command
(defun c:PVSTRINGS (/ oldlayer oldecho oldErr *undoStarted* doc blk eff ins ctr) 
  (setq oldlayer      (getvar "CLAYER")
        oldecho       (getvar "CMDECHO")
        doc           (vla-get-ActiveDocument (vlax-get-Acad-Object))
        *undoStarted* nil
        oldErr        *error*
  )
  (setq *error* (lambda (msg) 
                  (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*"))) 
                    (princ (strcat "\nError: " msg))
                  )
                  (if *undoStarted* (vla-EndUndoMark doc))
                  (if oldlayer (setvar "CLAYER" oldlayer))
                  (if oldecho (setvar "CMDECHO" oldecho))
                  (setq *error* oldErr)
                  (princ)
                )
  )
  (if (not (in-model-space-p)) 
    (princ "\nModel space only.")
    (progn 
      (setvar "CMDECHO" 0)
      (ensure-layer (pvw:cfg :layer) (pvw:cfg :layer-color))
      (vla-StartUndoMark doc)
      (setq *undoStarted* T)
      (setq blk (safe-entsel "Select a module block:"))
      (if (null blk) 
        (princ "\nCanceled.")
        (progn 
          (setq eff (get-effective-block-name blk)
                ins (inspt-of blk)
                ctr (pvw:block-center blk)
          )
          (princ (strcat "\nEffective name: " eff))
          (princ 
            (strcat "\nInsertion: " 
                    (rtos (car ins) 2 4)
                    ", "
                    (rtos (cadr ins) 2 4)
            )
          )
          (if ctr 
            (princ 
              (strcat "\nCenter   : " 
                      (rtos (car ctr) 2 4)
                      ", "
                      (rtos (cadr ctr) 2 4)
              )
            )
          )
          (princ "\n-- End slim mode --")
        )
      )
    )
  )
  (if *undoStarted* (vla-EndUndoMark doc))
  (if oldlayer (setvar "CLAYER" oldlayer))
  (if oldecho (setvar "CMDECHO" oldecho))
  (setq *error* oldErr)
  (princ)
)
