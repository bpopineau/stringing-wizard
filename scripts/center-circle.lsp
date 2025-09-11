;;; ==========================================================
;;; Center Circle              ;; Init                        ;; If first segment from block, use quadrant point on block; otherwise from previous center directly
                        (if (= centerPrev center) 
                          (progn
                            (setq quadPt (calculate-quadrant-point 
                                           centerPrev
                                           centerNext
                                           blockRadius
                                         )
                            )
                            (setq firstQuadPt quadPt) ; Store first quadrant point
                            (setq polyPoints (append polyPoints (list quadPt)))
                          )
                          (setq polyPoints (append polyPoints (list centerPrev)))
                        )
                        ;; Add the target center point to polyline
                        (setq polyPoints (append polyPoints (list centerNext)))
                        (princ 
                          (strcat "\nPoint added to polyline: " 
                                  (rtos (car centerNext) 2 4)
                                  ", "
                                  (rtos (cadr centerNext) 2 4)
                          )
                        )
                        (setq centerPrev centerNext)g loop
              (setq centerPrev center)
              (setq blockRadius (calculate-block-radius blockEnt center))
              (setq opt "")
              (setq polyPoints (list)) ; Initialize polyline points listl
;;; Description: Select an object and draw a 6" radius circle at its center
;;; Author: GitHub Copilot
;;; Date: September 11, 2025
;;; ==========================================================

;; Main command function
(defun c:CENTERCIRCLE (/ obj center oldlayer oldecho doc *undoStarted* oldErr 
                       blockEnt blockRadius quadPt obj2 center2 opt centerPrev objNext 
                       centerNext termBlockEnt termBlockRadius termQuadPt lastLineEnt 
                       lineData lineEnd lineStart newLineData ss testLine i 
                       continueLoop polyPoints firstQuadPt
                      ) 
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

      ;; Main loop for multiple string chains
      (setq continueLoop T)
      (while continueLoop 
        ;; Prompt user to select object
        (princ "\nSelect object to find center (or press ESC to exit): ")
        (setq obj (car (entsel)))

        (if (null obj) 
          (progn 
            (princ "\nNo object selected - exiting command.")
            (setq continueLoop nil)
          )
          (progn 
            ;; Calculate center of selected object
            (setq center (calculate-object-center obj))

            (if (null center) 
              (princ "\nCannot determine center of selected object.")
              (progn 
                ;; Insert StringTermPlus block at center
                (command "INSERT" 
                         "S:\\1 Jobs\\1 Current Jobs\\CAD Resources\\Blocks\\SLD and Stringing\\StringTermPlus.dwg" 
                         center 1.0 1.0 0
                )

                ;; Get the inserted block entity to calculate its actual size
                (setq blockEnt (entlast))

                (princ 
                  (strcat "\nStringTermPlus block inserted at center point: " 
                          (rtos (car center) 2 4)
                          ", "
                          (rtos (cadr center) 2 4)
                  )
                )

                ;; Initialize chain drawing loop
                (setq centerPrev center)
                (setq blockRadius (calculate-block-radius blockEnt center))
                (setq opt "")
                (while (not (wcmatch (strcase opt) "T")) 
                  (princ "\nSelect next object (Enter to continue, T to terminate): ")
                  (setq objNext (car (entsel)))
                  (if (null objNext) 
                    (progn 
                      (princ "\nNo object selected. Enter T to finish or select another object.")
                      (setq opt (getstring "\nType T to terminate or press Enter to continue: "))
                    )
                    (progn 
                      (setq centerNext (calculate-object-center objNext))
                      (if (null centerNext) 
                        (princ "\nCannot determine center of that object; skipping.")
                        (progn 
                          ;; If first segment from block, use quadrant point on block; otherwise from previous center directly
                          (if (= centerPrev center) 
                            (setq quadPt (calculate-quadrant-point 
                                           centerPrev
                                           centerNext
                                           blockRadius
                                         )
                            )
                            (setq quadPt centerPrev)
                          )
                          (command "LINE" quadPt centerNext "")
                          (princ 
                            (strcat "\nLine drawn to: " 
                                    (rtos (car centerNext) 2 4)
                                    ", "
                                    (rtos (cadr centerNext) 2 4)
                            )
                          )
                          (setq centerPrev centerNext)
                        )
                      )
                    )
                  )
                  (if (not (null objNext)) 
                    (setq opt (getstring "\nPress Enter to add another, or type T to terminate: "))
                  )
                )
                ;; When terminating, insert StringTermMinus block and adjust last line
                (if 
                  (and (wcmatch (strcase opt) "T") 
                       centerPrev
                       (not (equal centerPrev center))
                  )
                  (progn 
                    ;; Insert StringTermMinus block at last center point
                    (command "INSERT" 
                             "S:\\1 Jobs\\1 Current Jobs\\CAD Resources\\Blocks\\SLD and Stringing\\StringTermMinus.dwg" 
                             centerPrev 1.0 1.0 0
                    )
                    (setq termBlockEnt (entlast))
                    (setq termBlockRadius (calculate-block-radius 
                                            termBlockEnt
                                            centerPrev
                                          )
                    )

                    ;; Get the last line entity and modify its endpoint
                    (setq lastLineEnt (entlast))
                    ;; Find the line that ends at centerPrev (we need to search back)
                    (setq ss (ssget "X" '((0 . "LINE"))))
                    (if ss 
                      (progn 
                        (setq i (1- (sslength ss)))
                        (while (>= i 0) 
                          (setq testLine (ssname ss i))
                          (setq lineData (entget testLine))
                          (setq lineEnd (cdr (assoc 11 lineData)))
                          (if 
                            (and lineEnd 
                                 (< (distance lineEnd centerPrev) 0.001)
                            ) ; Very close to centerPrev
                            (progn 
                              (setq lineStart (cdr (assoc 10 lineData)))
                              ;; Calculate quadrant point on term block from line direction
                              (setq termQuadPt (calculate-quadrant-point 
                                                 centerPrev
                                                 lineStart
                                                 termBlockRadius
                                               )
                              )
                              ;; Modify the line endpoint
                              (setq newLineData (subst (cons 11 termQuadPt) 
                                                       (assoc 11 lineData)
                                                       lineData
                                                )
                              )
                              (entmod newLineData)
                              (setq i -1) ; Exit loop
                            )
                            (setq i (1- i))
                          )
                        )
                      )
                    )
                    (princ "\nStringTermMinus block inserted and line adjusted.")
                  )
                  (princ "\nChain terminated. Ready for next starting point.")
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
(defun calculate-object-center (ent / minPt maxPt vla minList maxList) 
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
    (progn 
      (setq minList (vlax-safearray->list minPt))
      (setq maxList (vlax-safearray->list maxPt))
      (list (/ (+ (car minList) (car maxList)) 2.0) 
            (/ (+ (cadr minList) (cadr maxList)) 2.0)
            (/ (+ (caddr minList) (caddr maxList)) 2.0)
      )
    )
    nil
  )
)

  ;; Helper function to calculate the radius of a block from its bounding box
(defun calculate-block-radius (blockEnt blockCenter / vla minPt maxPt minList maxList 
                               width height
                              ) 
  (setq vla (vlax-ename->vla-object blockEnt))
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
    (progn 
      (setq minList (vlax-safearray->list minPt))
      (setq maxList (vlax-safearray->list maxPt))
      (setq width (abs (- (car maxList) (car minList))))
      (setq height (abs (- (cadr maxList) (cadr minList))))
      ;; Return the larger of half-width or half-height as radius
      (/ (max width height) 2.0)
    )
    3.0 ;; Default fallback radius if calculation fails
  )
)

  ;; Helper function to calculate quadrant point on circle based on direction to target
(defun calculate-quadrant-point (center1 center2 radius / angle quadPt) 
  (setq angle (atan (- (cadr center2) (cadr center1)) 
                    (- (car center2) (car center1))
              )
  )
  (setq quadPt (list (+ (car center1) (* radius (cos angle))) 
                     (+ (cadr center1) (* radius (sin angle)))
                     (caddr center1)
               )
  )
  quadPt
)

  ;; Alternative command name for convenience
(defun c:CC () (c:CENTERCIRCLE))
(princ "\nCenter Circle Tool loaded. Type CENTERCIRCLE or CC to start.")
(princ)