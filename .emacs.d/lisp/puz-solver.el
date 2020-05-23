;;; package -- Summary
;;; Commentary:

;;; Code:

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

(defun puz-test(filePath)
  "Test for running puz stuff."
  (interactive "fFile name: ")
  (let ((puzContents (puz-parse-file filePath))
	(header nil)
	(solution nil)
	(fill nil)
	(current-pos 0))
    (setq header (bindat-unpack puz-header-spec puzContents))
    (print header)
    ;(print puzContents)
    ))



(provide 'puz-solver)
;;; puz-solver.el ends here
