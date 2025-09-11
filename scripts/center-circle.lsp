;;; ==========================================================
;;; Center Circle Tool
;;; Description: Select an object and draw a 6" radius circle at its center
;;; Author: GitHub Copilot
;;; Date: September 11, 2025
;;; ==========================================================

;; Main command function
(defun c:CENTERCIRCLE (/ obj center oldlayer oldecho doc *undoStarted* oldErr) 
  ;; Save system variables and set up error handling
  (setq oldlayer      (getvar "CLAYER")
        oldecho       (getvar "CMDECHO")
        doc           (vla-get-ActiveDocument (vlax-get-Acad-Object))
        *undoStarted* nil
        oldErr        *error*
  )

  ;; Define error handler for cleanup
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

  ;; Check if in model space
  (if (not (> (getvar "CVPORT") 1)) 
    (princ "\nMust be in Model Space to use this command.")
    (progn 
      ;; Set up environment
      (setvar "CMDECHO" 0)
      (vla-StartUndoMark doc)
      (setq *undoStarted* T)

      ;; Prompt user to select object
      (princ "\nSelect object to find center: ")
      (setq obj (car (entsel)))

      (if (null obj) 
        (princ "\nNo object selected.")
        (progn 
          ;; Calculate center of selected object
          (setq center (calculate-object-center obj))

          (if (null center) 
            (princ "\nCannot determine center of selected object.")
            (progn 
              ;; Draw 6" radius circle at center
              (command "CIRCLE" center 6.0)
              (princ 
                (strcat "\nCircle drawn at center point: " 
                        (rtos (car center) 2 4)
                        ", "
                        (rtos (cadr center) 2 4)
                )
              )
            )
          )
        )
      )

      ;; Cleanup
      (if *undoStarted* (vla-EndUndoMark doc))
      (if oldlayer (setvar "CLAYER" oldlayer))
      (if oldecho (setvar "CMDECHO" oldecho))
      (setq *error* oldErr)
    )
  )
  (princ)
)

;; Helper function to calculate the geometric center using bounding box method
(defun calculate-object-center (ent / minPt maxPt vla) 
  (setq vla (vlax-ename->vla-object ent))
  (if 
    (and vla 
         (not 
           (vl-catch-all-error-p 
             (vl-catch-all-apply 
               '(lambda () 
                  (vla-getboundingbox vla 'minPt 'maxPt)
                )
             )
           )
         )
    )
    (let 
      ((minList (vlax-safearray->list minPt)) 
        (maxList (vlax-safearray->list maxPt))
      )
      (list (/ (+ (car minList) (car maxList)) 2.0) 
            (/ (+ (cadr minList) (cadr maxList)) 2.0)
            (/ (+ (caddr minList) (caddr maxList)) 2.0)
      )
    )
    nil
  )
)

;; Alternative command name for convenience
(defun c:CC () (c:CENTERCIRCLE))

(princ "\nCenter Circle Tool loaded. Type CENTERCIRCLE or CC to start.")
(princ)