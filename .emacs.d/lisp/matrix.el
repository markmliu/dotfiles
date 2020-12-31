;;; package -- Matrix manipulation functions
;;; Commentary:

;;; Code:
(defun make-matrix (rows columns &optional initial)
  "Create a ROWS by COLUMNS matrix.  Optionally fills with INITIAL value."
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

(provide 'matrix)
;;; matrix.el ends here
