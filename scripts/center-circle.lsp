;;; ==========================================================
;;; Center Circle Tool - Solar String Planning
;;; Description: Select objects to create string paths with terminal blocks and polylines
;;; Author: GitHub Copilot
;;; Date: September 11, 2025
;;; ==========================================================

;; Main command function
(defun c:CENTERCIRCLE (/ obj center oldlayer oldecho doc *undoStarted* oldErr 
                       blockEnt blockRadius quadPt obj2 center2 opt centerPrev objNext 
                       centerNext termBlockEnt termBlockRadius termQuadPt lastLineEnt 
                       lineData lineEnd lineStart newLineData ss testLine i 
                       continueLoop lineEntities firstQuadPt oldPeditAccept
                      ) 
  ;; Save system variables and set up error handling
  (setq oldlayer       (getvar "CLAYER")
        oldecho        (getvar "CMDECHO")
        oldPeditAccept (getvar "PEDITACCEPT")
        doc            (vla-get-ActiveDocument (vlax-get-Acad-Object))
        *undoStarted*  nil
        oldErr         *error*
  )

  ;; Define error handler for cleanup
  (setq *error* (lambda (msg) 
                  (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*"))) 
                    (princ (strcat "\nError: " msg))
                  )
                  (if *undoStarted* (vla-EndUndoMark doc))
                  (if oldlayer (setvar "CLAYER" oldlayer))
                  (if oldecho (setvar "CMDECHO" oldecho))
                  (if oldPeditAccept (setvar "PEDITACCEPT" oldPeditAccept))
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
      (setvar "PEDITACCEPT" 1) ; Auto-accept PEDIT prompts like PolyTools
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
                (setq lineEntities (list)) ; Track line entities for joining
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
                          ;; Calculate line start point (from block edge or previous center)
                          (if (= centerPrev center) 
                            ;; First line from block - use quadrant point
                            (setq lineStart (calculate-quadrant-point 
                                              centerPrev
                                              centerNext
                                              blockRadius
                                            )
                            )
                            ;; Subsequent lines from previous center
                            (setq lineStart centerPrev)
                          )

                          ;; Create line from start point to next center
                          (command "PLINE" lineStart centerNext "")
                          (setq lineEntities (append lineEntities (list (entlast))))

                          (princ 
                            (strcat "\nLine created to: " 
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
                ;; After while loop ends, handle line joining first, then termination
                (if (>= (length lineEntities) 1) 
                  (progn 
                    ;; Join the line entities into a polyline using PolyTools technique
                    (if (> (length lineEntities) 1) 
                      (progn 
                        ;; Create selection set with all line entities
                        (setq ss (ssadd))
                        (foreach ent lineEntities 
                          (ssadd ent ss)
                        )

                        ;; Use PEDIT with multiple selection to join - PolyTools method
                        (command "_.pedit" "_m" ss "" "_j" "" "")
                        (princ "\nLines joined into polyline using PEDIT multiple.")
                      )
                      (progn 
                        ;; Single line - convert to polyline
                        (if (= (length lineEntities) 1) 
                          (progn 
                            (command "_.pedit" (car lineEntities) "" "")
                            (princ "\nSingle line converted to polyline.")
                          )
                        )
                      )
                    )

                    ;; Now handle termination block insertion and polyline adjustment
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

                        ;; Get the polyline that was just created (should be before the block)
                        (setq polylineEnt (entprev termBlockEnt))

                        ;; Verify it's a polyline, if not search backwards
                        (while 
                          (and polylineEnt 
                               (not 
                                 (equal (cdr (assoc 0 (entget polylineEnt))) 
                                        "LWPOLYLINE"
                                 )
                               )
                          )
                          (setq polylineEnt (entprev polylineEnt))
                        )

                        (if polylineEnt 
                          (progn 
                            ;; Calculate the quadrant point on the termination block
                            ;; Direction should be from the second-to-last point toward the block center
                            (setq polyData (entget polylineEnt))
                            (setq vertices (list))

                            ;; Collect all vertex points
                            (foreach item polyData 
                              (if (= (car item) 10) 
                                (setq vertices (append vertices (list (cdr item))))
                              )
                            )

                            ;; Get the second-to-last point for direction calculation
                            (if (> (length vertices) 1) 
                              (setq secondLastPt (nth (- (length vertices) 2) 
                                                      vertices
                                                 )
                              )
                              (setq secondLastPt (nth 0 vertices)) ; Fallback to first point
                            )

                            ;; Calculate quadrant point
                            (setq termQuadPt (calculate-quadrant-point 
                                               centerPrev
                                               secondLastPt
                                               termBlockRadius
                                             )
                            )

                            ;; Update the polyline's last vertex using PEDIT
                            (command "_.pedit" polylineEnt "_e" termQuadPt "")
                            (princ "\nPolyline endpoint adjusted to StringTermMinus block quadrant.")
                          )
                          (princ "\nWarning: Could not find polyline to adjust endpoint.")
                        )
                      )
                    )
                  )
                  (princ "\nNo line segments created.")
                )
              )
            )
          )
        )

        ;; Ask if user wants to create another string
        (if continueLoop 
          (progn 
            (princ "\nCreate another string? (Y/N): ")
            (setq opt (getstring))
            (if (not (wcmatch (strcase opt) "Y*")) 
              (setq continueLoop nil)
            )
          )
        )
      )

      ;; Cleanup
      (if *undoStarted* (vla-EndUndoMark doc))
      (if oldlayer (setvar "CLAYER" oldlayer))
      (if oldecho (setvar "CMDECHO" oldecho))
      (if oldPeditAccept (setvar "PEDITACCEPT" oldPeditAccept))
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