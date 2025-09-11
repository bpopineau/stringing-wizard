;;; ==========================================================
;;; Inverter & String Data Collection Tool
;;; Command: INVSTRINGS
;;; Purpose: Interactively gather inverter names, number of strings per inverter,
;;;          and number of modules per string.
;;; Returns: A global variable *INVSTRINGS_DATA* holding a structured list.
;;; Author: GitHub Copilot
;;; Date: 2025-09-11
;;; ==========================================================
;;; Data Structure (example):
;;;  (
;;;    ("Inverter A" ( (1 16) (2 15) (3 17) ))
;;;    ("Inverter B" ( (1 14) (2 14) ))
;;;  )
;;; Access last result: *INVSTRINGS_DATA*
;;; ==========================================================

(defun prompt-positive-integer (msg / val) 
  (while 
    (progn (princ msg) 
           (setq val (getint))
           (or (null val) (<= val 0))
    )
    (princ "\n  -> Enter a positive integer.")
  )
  val
)

(defun prompt-non-empty-string (msg / s) 
  (while 
    (progn (princ msg) 
           (setq s (getstring T))
           (or (null s) (= (strlen (vl-string-trim " \t" s)) 0))
    )
    (princ "\n  -> Value cannot be blank.")
  )
  (vl-string-trim " \t" s)
)

(defun format-inverter-summary (data / line acc inv name strings) 
  (setq acc (list "\nSummary:" "-----------"))
  (foreach inv data 
    (setq name    (car inv)
          strings (cadr inv)
    )
    (setq acc (append acc (list (strcat "  Inverter: " name))))
    (foreach pair strings 
      (setq acc (append acc 
                        (list 
                          (strcat "    String " 
                                  (itoa (car pair))
                                  ": "
                                  (itoa (cadr pair))
                                  " modules"
                          )
                        )
                )
      )
    )
    (apply 'strcat (mapcar '(lambda (x) (strcat x "\n")) acc))
  )

  (defun confirm (msg / ans) 
    (setq ans (strcase (vl-string-trim " \t" (getstring (strcat msg " (Y/N): ")))))
    (if (wcmatch ans "Y*") T nil)
  )

  (defun c:INVSTRINGS (/ inverterCount idx inverterName stringCount sIdx 
                       modulesPerString inverterData allData confirmed
                      ) 
    (princ "\n=== Inverter & String Data Collection ===")
    (setq confirmed nil)
    (while (not confirmed) 
      (setq allData '())
      (setq inverterCount (prompt-positive-integer "\nNumber of inverters: "))
      (setq idx 1)
      (while (<= idx inverterCount) 
        (setq inverterName (prompt-non-empty-string 
                             (strcat "  Inverter " (itoa idx) " name: ")
                           )
        )
        (setq stringCount (prompt-positive-integer 
                            (strcat "    Strings for '" inverterName "': ")
                          )
        )
        (setq sIdx 1)
        (setq inverterData (list inverterName '()))
        (while (<= sIdx stringCount) 
          (setq modulesPerString (prompt-positive-integer 
                                   (strcat "      Modules in string " 
                                           (itoa sIdx)
                                           ": "
                                   )
                                 )
          )
          (setq inverterData (list (car inverterData) 
                                   (append (cadr inverterData) 
                                           (list (list sIdx modulesPerString))
                                   )
                             )
          )
          (setq sIdx (1+ sIdx))
        )
        (setq allData (append allData (list inverterData)))
        (setq idx (1+ idx))
      )
      ;; Show verification summary
      (princ (format-inverter-summary allData))
      (if (confirm "Accept this data") 
        (setq confirmed T)
        (princ "\nRe-entering all data...")
      )
    )
    (setq *INVSTRINGS_DATA* allData)
    (princ (strcat "\nSaved. Total inverters: " (itoa (length allData))))
    (princ "\nUse (print *INVSTRINGS_DATA*) to view full structure.")
    (princ)
  )

  (princ "\nType INVSTRINGS to gather inverter and string data.")
  (princ)
)