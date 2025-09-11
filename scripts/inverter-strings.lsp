;;; ==========================================================
;;; Inverter & String Data Collection Tool
;;; Commands: INVSTRINGS, INVSTRINGSREPORT
;;; Purpose: Gather inverter names, string counts, modules per string with
;;;          confirmation; provide reporting of stored data.
;;; Global Result: *INVSTRINGS_DATA*
;;; Author: GitHub Copilot
;;; Date: 2025-09-11 (refactored)
;;; ==========================================================
;;; Data Structure Example (NEW with named strings):
;;;  (
;;;    ("INV-A" ( (1 "INV-A-1" 16) (2 "INV-A-2" 17) (3 "INV-A-3" 16) ))
;;;    ("INV-B" ( (1 "INV-B-1" 14) (2 "INV-B-2" 14) ))
;;;  )
;;; Legacy format without names still supported during reporting:
;;;    ("INV-A" ( (1 16) (2 17) ))
;;; ==========================================================

;; ---------------- Helper Prompts ----------------
;; Compact prompting helpers: use built-in prompt parameter to avoid
;; printing the same message + separate (princ). This keeps the command
;; line cleaner (AutoCAD cannot actually "un-print" earlier prompts).
(defun prompt-positive-integer (msg / val) 
  (while 
    (progn (setq val (getint (strcat "\n" msg))) 
           (or (null val) (<= val 0))
    )
    (princ "\n  -> Enter a positive integer.")
  )
  val
)

(defun prompt-non-empty-string (msg / s trimmed) 
  (while 
    (progn (setq s (getstring T (strcat "\n" msg))) 
           (setq trimmed (vl-string-trim " \t" s))
           (or (null s) (= (strlen trimmed) 0))
    )
    (princ "\n  -> Value cannot be blank.")
  )
  trimmed
)

(defun confirm (msg / ans) 
  (initget "Yes No")
  (setq ans (getkword (strcat "\n" msg " [Yes/No] <Yes>: ")))
  (if (null ans) (setq ans "Yes"))
  (eq ans "Yes")
)

;; ---------------- Data Utilities ----------------
(defun total-modules-in-inverter (inv / sum el) 
  (setq sum 0)
  (foreach el (cadr inv) 
    ;; Support both (idx modules) and (idx name modules)
    (setq sum (+ sum (if (= (length el) 2) (cadr el) (caddr el))))
  )
  sum
)

(defun format-inverter-summary (data / acc inv name strings tot grand el idx sname 
                                mods
                               ) 
  (setq acc (list "\nSummary:" "-----------"))
  (setq grand 0)
  (foreach inv data 
    (setq name    (car inv)
          strings (cadr inv)
          tot     (total-modules-in-inverter inv)
          grand   (+ grand tot)
    )
    (setq acc (append acc 
                      (list 
                        (strcat "  Inverter: " 
                                name
                                "  (Strings: "
                                (itoa (length strings))
                                ", Modules: "
                                (itoa tot)
                                ")"
                        )
                      )
              )
    )
    (foreach el strings 
      (cond 
        ((= (length el) 2) ; legacy (idx modules)
         (setq idx   (car el)
               sname (strcat name "-" (itoa idx))
               mods  (cadr el)
         )
        )
        ((= (length el) 3) ; new (idx name modules)
         (setq idx   (car el)
               sname (cadr el)
               mods  (caddr el)
         )
        )
        (T
         (setq idx   0
               sname "?"
               mods  0
         )
        )
      )
      (setq acc (append acc 
                        (list 
                          (strcat "    String " 
                                  (itoa idx)
                                  " ("
                                  sname
                                  ") : "
                                  (itoa mods)
                                  " modules"
                          )
                        )
                )
      )
    )
  )
  (setq acc (append acc 
                    (list "  ---" 
                          (strcat "  Total Modules (All Inverters): " (itoa grand))
                    )
            )
  )
  (apply 'strcat (mapcar '(lambda (x) (strcat x "\n")) acc))
)
  ;; ---------------- Main Command ----------------
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
                                         (list 
                                           (list sIdx 
                                                 (strcat inverterName 
                                                         "-"
                                                         (itoa sIdx)
                                                 )
                                                 modulesPerString
                                           )
                                         )
                                 )
                           )
        )
        (setq sIdx (1+ sIdx))
      )
      (setq allData (append allData (list inverterData)))
      (setq idx (1+ idx))
    )
    (princ (format-inverter-summary allData))
    (if (confirm "Accept this data") 
      (setq confirmed T)
      (princ "\nRe-entering all data...")
    )
  )
  (setq *INVSTRINGS_DATA* allData)
  (princ (strcat "\nSaved. Total inverters: " (itoa (length allData))))
  (princ "\nUse INVSTRINGSREPORT to print the stored summary later.")
  (princ)
)

  ;; ---------------- Report Command ----------------
(defun c:INVSTRINGSREPORT (/) 
  (if (and (boundp '*INVSTRINGS_DATA*) *INVSTRINGS_DATA*) 
    (progn (princ (format-inverter-summary *INVSTRINGS_DATA*)) (princ))
    (princ "\nNo inverter/string data stored yet. Run INVSTRINGS first.")
  )
  (princ)
)
(princ "\nType INVSTRINGS to gather inverter & string data; INVSTRINGSREPORT for summary.")
(princ)