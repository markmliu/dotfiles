;;; package -- Crossword editing mode
;;; Commentary:
;;; Follows NYT conventions - 15x15 or 21x21, 180 degree symmetry, each word two or more letters

;;; Code:
;;; TODO: get this working rather than explicitly importing
(require 'matrix)

(defun make-crossword (size)
  "Make a crossword grid with SIZE rows and columns."
  (if (zerop (% size 2))
      (error "Size must be odd for make-crossword"))
  (if (< size 3)
      (error "Size must be 3 or greater for make-crossword"))
  (make-matrix size size nil))

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

(defun crossword-cousin-ref (crossword row column)
  "Return cousin of CROSSWORD's ROW,COLUMN position."
  (let ((cousin-position (crossword-cousin-position crossword row column)))
    (crossword-ref crossword (car cousin-position) (cdr cousin-position))))

(defun crossword--cousin-set (crossword row column elt)
  "Set cousin of CROSSWORD's ROW,COLUMN position to ELT."
  (let ((cousin-position (crossword-cousin-position crossword row column)))
    (crossword--set crossword (car cousin-position) (cdr cousin-position) elt)))

;;; Crossword cell rules:
;;; Empty denoted by nil
;;; Block denoted by 'block
;;; Letter denoted by its ascii value (number)
;;; Semi-empty denoted by 'letter
(defun crossword-store-letter (crossword row column letter)
  "Given CROSSWORD, ROW, COLUMN, put LETTER there."
  (crossword--set crossword row column letter)
  (if (numberp (crossword-cousin-ref crossword row column))
      nil
    (crossword--cousin-set crossword row column 'letter)))

(defun crossword-store-block (crossword row column)
  "Given CROSSWORD, ROW, COLUMN, put 'block there."
  (crossword--set crossword row column 'block)
  (crossword--cousin-set crossword row column 'block))

;;; Erasing:
;;; If the cells cousin contains a letter, it becomes semi-empty
;;; Otherwise, it becomes empty and so does its cousin.
(defun crossword-clear-cell (crossword row column)
  "Given CROSSWORD, clear ROW and COLUMN."
  (let ((cousin-position (crossword-cousin-position crossword row column)))
    (if (and (not (equal cousin-position (cons row column))) (numberp (crossword-ref crossword (car cousin-position) (cdr cousin-position))))
	    ;;; if square is not its own cousin and cousin has a letter in it, make square semi-empty
	(crossword--set crossword row column 'letter)
      ;;; otherwise, make square nil
      (crossword--set crossword row column nil)
      (crossword--set crossword (car cousin-position) (cdr cousin-position) nil))))

(defun crossword-block-p (crossword row column)
  "Is CROSSWORD cell at ROW,COLUMN a block? If ROW,COLUMN outside of range, return t."
  (or (< row 0)
      (>= row (crossword-size crossword))
      (< column 0)
      (>= column (crossword-size crossword))
      (eq (crossword-ref crossword row column) 'block)))

(defun crossword-one-letter-p (crossword row column)
  "Is CROSSWORD cell at ROW,COLUMN a one-letter word?"
  (and (not (eq (crossword-ref crossword row column) 'block)) (or (and (crossword-block-p crossword (- row 1) column)
								       (crossword-block-p crossword (+ row 1) column))
								  (and (crossword-block-p crossword row (- column 1))
								       (crossword-block-p crossword row (+ column 1))))))


(defun crossword-insert-cell (cell)
  "Insert CELL into the current buffer."
  (insert (cond ((null cell) ".")
		((eq cell 'letter) "?")
		((eq cell 'block) "#")
		((numberp cell) cell)) " "))

(numberp 0)

(defun crossword-insert-row (row)
  "Insert ROW into the current buffer."
  (mapcar 'crossword-insert-cell row)
  (insert "\n"))

(defun crossword-insert-grid (crossword)
  "Insert CROSSWORD into the current buffer."
  (mapcar 'crossword-insert-row crossword))

;;; Assumes crossword is drawn and begins at (point-min)
(defun crossword-place-cursor (row column)
  "Move point to ROW,COLUMN."
  (goto-char (point-min))
  (forward-line row)
  (forward-char (* column 2)))

(defun current-line ()
  "Return line number containing point."
  (let ((result 1)) ;Emacs counts starting from 1
    (save-excursion
      (beginning-of-line)
      (while (not (bobp))
	(forward-line -1)
	(setq result (+ result 1))))
    result))

(defun crossword-cursor-coords ()
  "Compute (ROW . COLUMN) from cursor position."
  (cons (- (current-line) 1)
	(/ (current-column) 2)))

;;; Updating the display


(defun crossword-test ()
  "Try it out."
  (let ((test-crossword (make-crossword 5)))
    (crossword-store-letter test-crossword 1 4 ?A)
    ;;;(crossword-clear-cell test-crossword 1 4)
    (crossword-insert-grid test-crossword)
    ;;;(print test-crossword)
    ))

(defun crossword-update-display (crossword)
  "Called after a change, keep display up with CROSSWORD."
  (let* ((coords (crossword-cursor-coords))
	 (cousin-coords (crossword-cousin-position (car coords) (cdr coords))))
    (save-excursion
      (crossword-place-cursor (car coords)
			      (cdr coords))
      (delete-char 2)
      (crossword-insert-cell (crossword-ref crossword (car coords) (cdr coords)))
      (crossword-place-cursor (car cousin-coords)
			      (cdr cousin-coords))
      (delete-char 2)
      (crossword-insert-cell (crossword-ref crossword (car cousin-coords) (cdr cousin-coords))))))

;;; Assumes a buffer-local variable "crossword-grid"
(defun crossword-erase-command ()
  "Erase current crossword cell."
  (interactive)
  (let ((coords (crossword-cursor-coords)))
    (crossword-clear-cell crossword-grid (car coords) (cdr coords)))
  (crossword-update-display crossword-grid))

(defun crossword-block-command ()
  "Add block in current cell."
  (interactive)
  (let ((coords (crossword-cursor-coords)))
    (crossword-store-block crossword-grid (car coords) (cdr coords)))
  (crossword-update-display crossword-grid))

(defun crossword-self-insert ()
  "Self-insert letter into current cell."
  (interactive)
  (let ((coords (crossword-cursor-coords)))
    (crossword-store-letter crossword-grid (car coords) (cdr coords) (aref (this-command-keys) 0)))
  (crossword-update-display crossword-grid))

;;; movement
(defun crossword-cursor-right (arg)
  "Move ARG cells to the right."
  (interactive "p")
  (let* ((coords (crossword-cursor-coords))
	 (new-column (+ arg (cdr coords))))
    (if (or (< new_column 0) (>= new_column (crossword-size crossword-grid)))
	(error "Out of bound."))
    (crossword-place-cursor (car coords) new-column)))

(defun crossword-cursor-left (arg)
  "Move ARG cells to the left."
  (interactive "p")
  (crossword-cursor-right (- arg)))

(defun crossword-cursor-down (arg)
  "Move ARG cells down."
  (interactive "p")
  (let* ((coords (crossword-cursor-coords))
	 (new-row (+ arg (cdr coords))))
    (if (or (< new-row 0) (>= new-row (crossword-size crossword-grid)))
	(error "Out of bound."))
    (crossword-place-cursor new-row (cdr coords))))

(defun crossword-cursor-up (arg)
  "Move ARG cells up."
  (interactive "p")
  (crossword-cursor-down (- arg)))

(defun crossword-beginning-of-row ()
  "Move to beginning of current row."
  (interactive)
  (let ((coords (crossword-cursor-coords)))
    (crossword-place-cursor (car coords) 0)))

(defun crossword-end-of-row ()
  "Move to end of current row."
  (interactive)
  (let ((coords (crossword-cursor-coords)))
    (crossword-place-cursor (car coords) (- (crossword-size crossword-grid) 1))))

(defun crossword-top-of-column ()
  "Move to top of current column."
  (interactive)
  (let ((coords (crossword-cursor-coords)))
    (crossword-place-cursor 0 (cdr coords))))

(defun crossword-bottom-of-column ()
  "Move to bottom of current column."
  (interactive)
  (let ((coords (crossword-cursor-coords)))
    (crossword-place-cursor (- (crossword-size crossword-grid) 1 )(cdr coords))))

(defun crossword-beginning-of-grid ()
  "Move to beginning of grid."
  (interactive)
  (crossword-place-cursor 0 0))

(defun crossword-end-of-grid ()
  "Move to end of grid."
  (interactive)
  (let ((end-index ( - (crossword-size crossword-grid) 1)))
    (crossword-place-cursor end-index end-index)))







;;;(crossword-test)

(provide 'puz-ed)
;;; puz-ed.el ends here
