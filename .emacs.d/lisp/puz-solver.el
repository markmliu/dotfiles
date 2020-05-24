;;; package -- Summary
;;; Commentary:

;;; Code:
(require 'cl)

(defun puz-mode()
  "Major mode for reading and editing puz files."
  (interactive)
  (puz--mode-setup (puz-parse-buffer)))

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

(defun puz-parse-file(filePath)
  "Helper for reading .puz FILEPATH into a unibyte string."
  (with-temp-buffer
    (insert-file-contents filePath)
    (encode-coding-string (buffer-string) 'binary)))

(defvar puz--read-and-inc nil
  "Used to store a closure for reading and incrementing.")

(cl-defstruct crossword-info header solution fill height width title author copyright clues)

(defun puz-test(filePath)
  "Test for running puz stuff."
  (interactive "fFile name: ")
  (let ((puz-content (puz-parse-file filePath))
	(header nil)
	(solution nil)
	(fill nil)
	(current-pos 0)
	(height 0)
	(width 0)
	(title "")
	(author "")
	(copyright "")
	(clues nil))
    (setq header (bindat-unpack puz-header-spec puz-content))
    (setq current-pos (bindat-length puz-header-spec header))
    (setq height (cdr (assoc 'height header)))
    (setq width (cdr (assoc 'width header)))
    (let ((solution-spec (list (list 'raw 'str (* height width)))))
      ;; read the solution and advance current-pos
      (setq solution (bindat-unpack solution-spec puz-content current-pos))
      (incf current-pos (* height width ))
      ;; read the fill and advance current-pos
      (setq fill (bindat-unpack solution-spec puz-content current-pos))
      (incf current-pos (* height width ))
      )

    (print solution)
    (print fill)
    ;; solution stored in solution.raw
    ;; fill stored in fill.raw

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

    (setq title (funcall puz--read-and-inc))
    (setq author (funcall puz--read-and-inc))
    (setq copyright (funcall puz--read-and-inc))

    ;; (print title)
    ;; (print author)
    ;; (print copyright)
    (let ((numclues (cdr (assoc 'numclues header))))
      (dotimes (i numclues)
	(setq clues (cons (funcall puz--read-and-inc) clues))))
    (setq clues (nreverse clues))
    (print clues)

    ))




(provide 'puz-solver)
;;; puz-solver.el ends here
