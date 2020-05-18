;;; package -- Crossword editing mode
;;; Commentary:
;;; Follows NYT conventions - 15x15 or 21x21, 180 degree symmetry, each word two or more letters

;;; Code:
(require 'matrix)
(defun make-crossword (size)
  "Make a crossword grid with SIZE rows and columns."
  (if (zerop (% size 2))
      (error "Size must be odd for make-crossword"))
  (if (< size 3)
      (error "Size must be 3 or greater for make-crossword"))
  (make-matrix (size size nil))
  )

(make-crossword 5)

(defsubst crossword-size (crossword)
  "Return size of CROSSWORD."
  (matrix-rows crossword))

(defsubst crossword-ref (crossword row column)
  "Return element of CROSSWORD at ROW,COLUMN."
  (matrix-ref crossword row column))

(defsubst crossword--set (crossword row column elt)
  "Internal function for setting CROSSWORD at ROW,COLUMN to ELT."
  (matrix-set crossword row column elt))

(defun crossword-cousin-position (crossword row column)
  "Give the 'cousin' position for CROSSWORD ROW and COLUMN."
  (let ((size (crossword-size crossword)))
    (cons (- size row 1) (- size column 1))))

(defun crossword-test ()
  "Try it out."
  (let ((test-crossword (make-crossword 5)))
    (print (crossword-cousin-position test-crossword 1 4))
    ))

(crossword-test)

(provide 'puz-ed)
;;; puz-ed.el ends here
