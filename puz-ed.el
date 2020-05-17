;;; package -- Crossword editing mode
;;; Commentary:
;;; Follows NYT conventions - 15x15 or 21x21, 180 degree symmetry, each word two or more letters

;;; Code:
(defsubst make-matrix (rows columns &optional initial)
  "Create a ROWS by COLUMNS matrix."
  (let ((result (make-vector rows nil))
	(y 0))
    (while (< y rows)
	   (aset result y (make-vector columns initial))
	   (setq y (+ y 1)))
    result))

(defsubst matrix-set (matrix row column elt)
  "Put ELT in MATRIX at ROW,COLUMN."
  (let ((vec (aref matrix row)))
    (aset vec column elt)))

(defsubst matrix-ref (matrix row column)
  "Get element of MATRIX at ROW,COLUMN."
  (let ((vec (aref matrix row)))
    (aref vec column)))

(defsubst matrix-columns (matrix)
  "Number of columns in MATRIX.  Requires MATRIX be non-empty."
  (length (aref matrix 0)))

(defsubst matrix-rows (matrix)
  "Number of rows in MATRIX.  Requires MATRIX be non-empty."
  (length matrix))

(provide 'puz-ed)
;;; puz-ed.el ends here
