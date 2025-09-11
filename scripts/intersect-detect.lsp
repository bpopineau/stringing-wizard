;;; ---------------------------------------------------------------------------
;;; intersect-detect.lsp
;;; Utility to count how many UNIQUE drawing objects a selected polyline
;;; geometrically intersects (touches at one or more points) using the
;;; ActiveX IntersectWith method.
;;;
;;; Public Command:
;;;   PLXINT  - Prompts for a single lightweight (or old style) polyline and
;;;             reports the number of unique objects it intersects. Optionally
;;;             creates a temporary selection set highlighting them.
;;;
;;; Notes / Limitations:
;;; - Uses a crossing window of the polyline's bounding box to limit the
;;;   candidate set for performance. If your drawing has objects that extend
;;;   through the polyline area without crossing its bounding box corners,
;;;   you may wish to broaden the search (see comment below).
;;; - Counts an object once even if multiple intersection points exist.
;;; - Overlapping / colinear situations (e.g., a polyline segment perfectly
;;;   lying on a line) may or may not yield intersection points from
;;;   IntersectWith depending on object types; a secondary geometric test is
;;;   included for some curve-on-curve overlap cases.
;;; - Locked / off layers are still examined (change the selection filter
;;;   section to exclude if desired).
;;; - Tested in AutoCAD flavors supporting Visual LISP (VLAX) functions.
;;;
;;; ---------------------------------------------------------------------------

(vl-load-com) ; Ensure Visual LISP ActiveX is available

;; Enumeration constant (guard so we don't overwrite if user already defined)
(if (not (boundp 'acExtendNone)) (setq acExtendNone 0))

;;; Helper: Convert a VARIANT or SAFEARRAY (of doubles) to a plain list
(defun _var->list (v) 
  (cond 
    ((= (type v) 'VARIANT) (vlax-safearray->list (vlax-variant-value v)))
    ((= (type v) 'SAFEARRAY) (vlax-safearray->list v))
    ((listp v) v)
    (t nil)
  )
)

;;; Helper: Ensure 3D point (z defaults to 0.0)
(defun _3d (pt) 
  (list (car pt) (cadr pt) (if (caddr pt) (caddr pt) 0.0))
)

;;; Helper: quick overlap heuristic for colinear / coincident objects where
;;; IntersectWith may return no points (e.g., fully overlapping segments)
(defun _maybe-overlap? (o1 o2) 
  (and (vlax-method-applicable-p o1 'GetClosestPointTo) 
       (vlax-method-applicable-p o2 'GetClosestPointTo)
       (let* 
         ((p1 (vlax-curve-getStartPoint o1)) 
           (qraw (vlax-invoke o2 'GetClosestPointTo (vlax-3d-point p1)))
           (p2 (_var->list qraw))
           (d (if (and p1 p2) (distance p1 p2) 1e9))
         )
         (< d 1e-9)
       )
  ) ; tolerance
)

;;; Core routine: given a VLA polyline object, return list of (ename . handle)
(defun plxint-collect (vlaPoly / minVar maxVar minPt maxPt ss cnt i en other raw ints 
                       handles result h
                      ) 
  ;; Acquire bounding box & candidate selection
  (vla-GetBoundingBox vlaPoly 'minVar 'maxVar)
  (setq minPt (_3d (vlax-safearray->list (vlax-variant-value minVar)))
        maxPt (_3d (vlax-safearray->list (vlax-variant-value maxVar)))
  )
  ;; Candidate set: crossing of bounding box. To broaden, replace with (ssget "X").
  (setq ss      (ssget "C" minPt maxPt)
        handles '()
        result  '()
  )
  (when 
    ss
    (setq cnt (sslength ss)
          i   0
    )
    (while (< i cnt) 
      (setq en (ssname ss i)
            i  (1+ i)
      )
      (if (/= (vlax-vla-object->ename vlaPoly) en)  ; skip self
        (progn 
          (setq other (vlax-ename->vla-object en))
          (setq raw (vl-catch-all-apply 'vlax-invoke 
                                        (list vlaPoly 
                                              'IntersectWith
                                              other
                                              acExtendNone
                                        )
                    )
          )
          (cond 
            ((and raw (not (vl-catch-all-error-p raw)))
             (setq ints (_var->list raw))
             (if (or ints (_maybe-overlap? vlaPoly other)) 
               (progn 
                 (setq h (strcase (vla-get-Handle other)))
                 (if (not (member h handles)) 
                   (setq handles (cons h handles)
                         result  (cons (cons en h) result)
                   )
                 )
               )
             )
            )
            (t ; IntersectWith failed; attempt overlap heuristic
             (if (_maybe-overlap? vlaPoly other) 
               (progn 
                 (setq h (strcase (vla-get-Handle other)))
                 (if (not (member h handles)) 
                   (setq handles (cons h handles)
                         result  (cons (cons en h) result)
                   )
                 )
               )
             )
            )
          )
        )
      )
    )
    (reverse result)
  )
)
  ;;; Public command (top-level)
(defun c:PLXINT (/ sel e vlaPoly pairs count setname) 
  (princ "\nSelect a single polyline to analyze: ")
  (setq sel (entsel))
  (cond 
    ((null sel) (princ "\nNothing selected."))
    (t
     (setq e (car sel))
     (setq vlaPoly (vlax-ename->vla-object e))
     (if (not (wcmatch (strcase (vla-get-ObjectName vlaPoly)) "*POLYLINE")) 
       (princ "\nEntity is not a polyline.")
       (progn 
         (princ "\nProcessing intersections...")
         (setq pairs (plxint-collect vlaPoly)
               count (length pairs)
         )
         (princ (strcat "\nUnique intersecting objects: " (itoa count)))
         (if (> count 0) 
           (progn 
             ;; Build a selection set for user convenience
             (setq setname (strcat "PLXINT_" (vla-get-Handle vlaPoly)))
             (princ "\nCreating temporary selection set...")
             (sssetfirst nil (ssadd)) ; clear grips
             (foreach pr pairs 
               (ssadd (car pr) 
                      (setq *plxint-last-ss* (if *plxint-last-ss* 
                                               *plxint-last-ss*
                                               (ssadd)
                                             )
                      )
               )
             )
             (if *plxint-last-ss* (sssetfirst nil *plxint-last-ss*))
             (setq *plxint-last* (mapcar 'cdr pairs))
             (princ "\nHandles stored in *plxint-last* variable.")
           )
         )
       )
     )
    )
    (princ)
  )
)
  ;;; Quiet load message
(princ "\nPLXINT loaded. Use PLXINT to count polyline intersections.")
(princ)