;;; package -- Summary
;;; Commentary:

;;; Code:
(require 'cl)

(defun current-line ()
  "Return line number containing point."
  (let ((result 1)) ;Emacs counts starting from 1
    (save-excursion
      (beginning-of-line)
      (while (not (bobp))
	(forward-line -1)
	(setq result (+ result 1))))
    result))

(defun puz-cursor-coords ()
  "Compute (ROW . COLUMN) from cursor position."
  (cons (- (current-line) 1) (current-column) ))

(defun puz-cursor-index (width)
  "Compute index from cursor position for grid of WIDTH."
  (+ (* (current-line) width) (current-column)))

(defun puz-col (index width)
  "Compute column, given INDEX into fill and WIDTH."
  (mod index width))

(defun puz-row (index width)
  "Compute row, given INDEX into fill and WIDTH."
  (/ index width))

(defun puz-is-blacksquare (c)
  "Return whether C represents a black square."
  (interactive)
  (eq c ?.))

(defun puz-grid-at-index (index grid)
  "Compute value of GRID at INDEX."
  (aref grid index))

(defun puz-length-across (index grid width)
  "Compute length of an across clue starting from INDEX for GRID of WIDTH."
  (let ((len-so-far 0) ;length of clue
	(cur-col (puz-col index width))
	(cur-char nil)
	(reached nil)); whether we've reached a black square yet)
    (print grid)
    (while (and (<= cur-col width) (not reached))
      (setq cur-char (puz-grid-at-index (+ index len-so-far) grid ))

      ;;(print (message "%c" cur-char))
      (setq reached (puz-is-blacksquare cur-char))
      (incf len-so-far)
      (incf cur-col))
    (- len-so-far 1)
    ))

(defvar puz-header-spec
      '((cksum_gbl u16r)
	(across_down str 11)
	(fill 1)
	(cksum_hdr u16r)
	;; gotta figure out cksum_magic (Q - unsigned long long)
	;; cheat for now and just skip it
	(fill 8)
	(fileversion str 4)
	(unk1 str 2)
	(scrambled_cksum u16r)
	(unk2 str 12)
	(width byte)
	(height byte)
	(numclues u16r)
	(puzzletype u16r)
	(solution_state u16r)))

(defun puz-get-bytestring(filePath)
  "Helper for reading .puz FILEPATH into a unibyte string."
  (with-temp-buffer
    (insert-file-contents filePath)
    (encode-coding-string (buffer-string) 'binary)))

(defvar puz--read-and-inc nil
  "Used to store a closure for reading and incrementing.")

(cl-defstruct crossword-info header solution fill height width title author copyright clues)

(defun puz-parse (filePath)
  "Pull all crossword info out of .puz file at FILEPATH."
  (let ((puz-content (puz-get-bytestring filePath))
	(current-pos 0)
	(puz-info (make-crossword-info :header nil :solution nil :fill nil :height 0 :width 0 :title "" :author "" :copyright "" :clues nil)))
    (setf (crossword-info-header puz-info) (bindat-unpack puz-header-spec puz-content))
    (setq current-pos (bindat-length puz-header-spec (crossword-info-header puz-info)))
    (setf (crossword-info-height puz-info) (cdr (assoc 'height (crossword-info-header puz-info))))
    (setf (crossword-info-width puz-info) (cdr (assoc 'width (crossword-info-header puz-info))))
    (let ((solution-spec (list (list 'raw 'str (* (crossword-info-height puz-info) (crossword-info-width puz-info))))))
      ;; read the solution and advance current-pos
      (setf (crossword-info-solution puz-info) (cdr (assoc 'raw (bindat-unpack solution-spec puz-content current-pos))))
      ;; (setf (crossword-info-solution puz-info) (bindat-unpack solution-spec puz-content current-pos))
      (incf current-pos (* (crossword-info-height puz-info) (crossword-info-width puz-info)))
      ;; read the fill and advance current-pos
      (setf (crossword-info-fill puz-info) (cdr (assoc 'raw (bindat-unpack solution-spec puz-content current-pos))))
      ;; (setf (crossword-info-fill puz-info) (bindat-unpack solution-spec puz-content current-pos))
      (incf current-pos (* (crossword-info-height puz-info) (crossword-info-width puz-info)))
      )

    ;; (print (crossword-info-solution puz-info))
    ;; (print (crossword-info-fill puz-info))
    (setq puz--read-and-inc (lambda ()
			      (let ((next_null_pos current-pos)
				    (string_so_far ""))
				(while (not (eq
					     (aref (substring puz-content next_null_pos (+ 1 next_null_pos)) 0)
					     0))
				  (setq string_so_far
					(concat string_so_far
						(substring puz-content next_null_pos (+ 1 next_null_pos))))
				  (incf next_null_pos 1))
				(setq current-pos (+ next_null_pos 1))
				string_so_far
				)))

    (setf (crossword-info-title puz-info) (funcall puz--read-and-inc))
    (setf (crossword-info-author puz-info) (funcall puz--read-and-inc))
    (setf (crossword-info-copyright puz-info) (funcall puz--read-and-inc))

    ;; (print title)
    ;; (print author)
    ;; (print copyright)
    (let ((numclues (cdr (assoc 'numclues (crossword-info-header puz-info)))))
      (dotimes (i numclues)
	(setf (crossword-info-clues puz-info) (cons (funcall puz--read-and-inc) (crossword-info-clues puz-info)))))
    (setf (crossword-info-clues puz-info) (nreverse (crossword-info-clues puz-info)))
    ;; (print (crossword-info-clues puz-info))

    puz-info
    ))

(setq testvar (puz-parse "test.puz"))
(crossword-info-fill testvar)

(defun puz-insert-fill (fill height width)
  "Insert FILL into buffer, assuming cursor is at top of buffer and FILL is of size HEIGHT * WIDTH."
  (let ((count 0)
	(start-ind 0)
	(end-ind width))
    (while (< count height)
      (insert (concat (substring fill start-ind end-ind) "\n"))
      (incf start-ind width)
      (incf end-ind width)
      (incf count 1))))

(defun puz-insert-clues (clues)
  "Insert CLUES into buffer."
  (while clues
    (insert (concat (car clues) "\n"))
    (setq clues (cdr clues))))

;; have to define puz-forward-word and puz-backward-word
(defun puz-forward-word ()
  "In puz-solving buffer, move cursor forward to next valid word."
  (interactive)
  (while (not (eq (char-after (point)) ?.))
    (forward-char))
  (while (eq (char-after (point)) ?.)
    (forward-char)))

(defun puz-backward-word ()
  "In puz-solving buffer, move cursor backward to next valid word."
  (interactive)
  (while (not (eq (char-before (point)) ?.))
    (backward-char))
  (while (eq (char-before (point)) ?.)
    (backward-char)))

(defvar puz-mode-map nil
  "Keymap for puz mode.")
(if puz-mode-map nil
  (setq puz-mode-map (make-keymap))
  (let ((equivs
	 '((forward-word . puz-forward-word)
	   (backward-word . puz-backward-word))))
    (while equivs
      (substitute-key-definition (car (car equivs))
				 (cdr (car equivs))
				 puz-mode-map
				 (current-global-map))
      (setq equivs (cdr equivs))))
  )

(defun puz-mode(filePath)
  "Create a new buffer with graphical view of .puz file at FILEPATH."
  (interactive "fFile name: ")
  (kill-all-local-variables)
  (setq mode-name "Crossword Solving")
  (let ((puz-info (puz-parse filePath))
	(grid-buffer (generate-new-buffer "*Grid*"))
	(clues-buffer (generate-new-buffer "*Clues*")))

    (delete-other-windows)
    (switch-to-buffer grid-buffer)
    (puz-insert-fill (crossword-info-fill puz-info)
		     (crossword-info-height puz-info)
		     (crossword-info-width puz-info))
    (split-window-right)
    (other-window 1)
    (switch-to-buffer clues-buffer)
    (puz-insert-clues (crossword-info-clues puz-info))
    (other-window 1)
    (use-local-map puz-mode-map)
    ))

;; test stuff below

(defun puz-test(index)
  "Test."
  (let* ((puz-info   (puz-parse "test.puz"))
	 (len (length (crossword-info-fill puz-info)))
	 (i 0))
    ;; try numbering stuff

    (puz-length-across index (crossword-info-fill puz-info) (crossword-info-width puz-info))



  ))

(puz-test 12)
(puz-test 26)
;;(lookup-key (current-global-map) (kbd "M-b "))

(provide 'puz-solver)
;;; puz-solver.el ends here
