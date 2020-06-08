;;; package -- Summary
;;; Commentary:

;;; Code:
(require 'cl)

(defun current-line ()
  "Return line number containing point."
  (let ((result 0))
    (save-excursion
      (beginning-of-line)
      (while (not (bobp))
	(forward-line -1)
	(setq result (+ result 1))))
    result))

(defun puz-cursor-coords ()
  "Compute (ROW . COLUMN) from cursor position."
  (cons (current-line) (current-column) ))

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
  (if (>= index (length grid))
      0 ; early return if we happen to index too far
    (let ((len-so-far 0) ;length of clue
	  (cur-col (puz-col index width))
	  (cur-char nil)
	  (reached nil)); whether we've reached a black square yet)
      (print grid)
      (while (and (< cur-col width) (not reached))
	(setq cur-char (puz-grid-at-index (+ index len-so-far) grid ))

	;;(print (message "%c" cur-char))
	(setq reached (puz-is-blacksquare cur-char))
	(incf len-so-far)
	(incf cur-col))
      ;; two options, either ran out of space or ran into terminating block
      (if reached
	  (- len-so-far 1)
	len-so-far)
      )))

(defun puz--length-of-blacksquares-across (index grid width)
  "Compute number of continuous black squares from INDEX for GRID of WIDTH.
Assumes that index is the start of a section rather than the middle."
  (if (>= index (length grid))
      0
    (let ((len-so-far 0) ;length of black square section
	  (cur-col (puz-col index width))
	  (cur-char nil)
	  (reached nil)); whether we've reached a non-black square yet)
      (print grid)
      (while (and (< cur-col width) (not reached))
	(setq cur-char (puz-grid-at-index (+ index len-so-far) grid ))

	;;(print (message "%c" cur-char))
	(setq reached (not (puz-is-blacksquare cur-char)))
	(incf len-so-far)
	(incf cur-col))
      (if reached
	  (- len-so-far 1)
	len-so-far)
      )))


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

(cl-defstruct clues-info ; info about set of clues - one for across and one for down.
  starts    ; start indexes for clues
  lengths   ; lengths for clues
  )

(cl-defstruct crossword-info
  header            ; crossword header
  solution          ; crossword grid - filled in with solution
  fill              ; crossword grid - contains whatever progress has been made
  height            ; height of grid
  width             ; width of grid
  title             ;
  author            ;
  copyright         ;
  clues             ; raw clues - need to be numbered
  across            ; info about across clue
  down              ; info about down clues
  )

(defvar puz-info nil
  "Used to store info about the puz file.")
(if puz-info nil
  (setq puz-info (make-crossword-info
		  :header nil
		  :solution nil
		  :fill nil
		  :height 0
		  :width 0
		  :title ""
		  :author ""
		  :copyright ""
		  :clues nil
		  :across (make-clues-info
			   :starts nil
			   :lengths nil)
		  :down (make-clues-info
			 :starts nil
			 :lengths nil))))

(defun puz-index-at-cursor ()
  "Compute index from cursor position for grid of WIDTH."
  (interactive)
  (let ((width (crossword-info-width puz-info)))
    (print (+ (* (current-line) width) (current-column)))
    ))

(defun puz-across-at-cursor ()
  "Index of across clue at cursor (one-indexed and not zero)."
  (interactive)
  (let ((index (puz-index-at-cursor))
	(across-starts (clues-info-starts (crossword-info-across puz-info)))
	(cur 0))
    (while (<= (aref across-starts cur) index)
      (incf cur))
    (print cur)
    (print (aref (vconcat (crossword-info-clues puz-info)) (- cur 1)))))

(defvar puz--show-across-or-down t
  "If true, shows across clues.  If false, shows downs.")


;; (add-hook 'post-command-hook 'puz-across-at-cursor :local)
;; TODO: put puz-across-at-cursor into some hook. but this hook should
;; allow displaying either across or down clues

(defvar puz-mode-hook nil
  "*List of functions to call when entering puz-mode.")

(defun crossword--across-info-from-grid (grid width)
  "Get info about across clues from GRID with WIDTH."
  (let ((len (length grid))
	(i 0)
	(across (make-clues-info
		 :starts '()
		 :lengths '())))
    ; find the first non-black square
    (while (puz-is-blacksquare (puz-grid-at-index i grid))
      (incf i)
      )
    (while (< i len)
      (let* ((clue-length (puz-length-across i grid width))
	     (black-squares-length (puz--length-of-blacksquares-across
				    (+ i clue-length)
				    grid
				    width)))

	(if (eq 0 clue-length)
	    (error (format "0 length clue at index %d after finding %d clues where char is %c"
			   i
			   (length (clues-info-lengths across))
			   (puz-grid-at-index i grid)
			   )))

	(setf (clues-info-starts across)
	      (cons i (clues-info-starts across)))
	(print i)
	(incf i (+ clue-length black-squares-length))))
    (setf (clues-info-starts across)
	  (vconcat (nreverse (clues-info-starts across))))
    across
    ))

(defun puz-parse-current-file ()
  "Call puz-parse on currently open file."
  (puz-parse (buffer-file-name)))

(defun puz-parse (filePath)
  "Pull all crossword info out of .puz file at FILEPATH."
  (let ((puz-content (puz-get-bytestring filePath))
	(current-pos 0))
    (setf (crossword-info-header puz-info) (bindat-unpack puz-header-spec puz-content))
    (setq current-pos (bindat-length puz-header-spec (crossword-info-header puz-info)))
    (setf (crossword-info-height puz-info) (cdr (assoc 'height (crossword-info-header puz-info))))
    (setf (crossword-info-width puz-info) (cdr (assoc 'width (crossword-info-header puz-info))))
    (let ((solution-spec (list (list 'raw 'str (* (crossword-info-height puz-info) (crossword-info-width puz-info))))))
      ;; read the solution and advance current-pos
      (setf (crossword-info-solution puz-info)
	    (cdr (assoc 'raw (bindat-unpack solution-spec puz-content current-pos))))
      (incf current-pos (* (crossword-info-height puz-info) (crossword-info-width puz-info)))
      ;; read the fill and advance current-pos
      (setf (crossword-info-fill puz-info)
	    (cdr (assoc 'raw (bindat-unpack solution-spec puz-content current-pos))))
      (incf current-pos (* (crossword-info-height puz-info) (crossword-info-width puz-info)))
      )

    ;; (print (crossword-info-solution puz-info))
    ;; (print (crossword-info-fill puz-info))
    (setq puz--read-and-inc
	  (lambda ()
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

    (setf (crossword-info-across puz-info)
	  (crossword--across-info-from-grid
	   (crossword-info-fill puz-info)
	   (crossword-info-width puz-info)))
    puz-info
    ))

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
  (while (and (not (eq (char-after (point)) ?.)) (not (eq (point) (line-end-position))))
    (forward-char))
  (while (and (eq (char-after (point)) ?.) (not (eq (point) (line-end-position))))
    (forward-char))
  (if (eq (point) (line-end-position))
      (forward-char))
  )

(defun puz-backward-word ()
  "In puz-solving buffer, move cursor backward to next valid word."
  (interactive)
  (while (and (not (eq (char-before (point)) ?.)) (not (eq (point) (line-beginning-position))))
    (backward-char))
  (while (and (eq (char-before (point)) ?.) (not (eq (point) (line-beginning-position))))
    (backward-char))
  (if (eq (point) (line-beginning-position))
      (backward-char))
  )

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

(defun puz-mode()
  "Create a new buffer with graphical view of .puz file at FILEPATH."
  (kill-all-local-variables)
  (setq major-mode 'puz-mode
  (setq mode-name "Crossword Solving")
  (let ((puz-info (puz-parse-current-file))
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
    (run-hooks 'puz-mode-hook)
    ))

;; test stuff below

(defun puz-test()
  "Test."
  (puz-parse "test.puz")
  (print (clues-info-starts (crossword-info-across puz-info)))
    ;; (print (crossword-info-across puz-info))
    ;; (puz-length-across 12
    ;; 		       (crossword-info-fill puz-info)
    ;; 		       (crossword-info-width puz-info))
  )

(puz-test)
;;(lookup-key (current-global-map) (kbd "M-b "))

(provide 'puz)
;;; puz.el ends here
