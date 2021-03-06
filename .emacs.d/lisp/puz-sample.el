;;; package -- Summary
;;; Commentary:

;;; Code:

(defvar puz-mode-content ",AACROSS&DOWN êK3ÝÝ«~Ä1.3      nytimes.comN    APSE..BLAB..TUTBOOTY.TAIL.BUNSSERTA.WIDE.ERIKCHEEKS.REAREND.OLA.SEZ.DRAFTEDNERF.XES.ITSWARDRMOM.SHOES.OLE...BOTTOMROW...PSS.RAYON.NEIGHOKTHEN.TIE.ASIAMIRANDA.ABS.ANN.CABOOSE.BEHINDALII.OKRA.REDIDBUTT.RUMP.FANNYCBS..IPAS..LOGE----..----..--------.----.---------.----.----------.-------.---.---.-----------.---.-----------.-----.---...---------...---.-----.-----------.---.-----------.---.---.-------.----------.----.---------.----.--------..----..----NY Times, Monday, May 11, 2020  Ross Trudeau / Will Shortz © 2020, The New York Times Chapel recess Leave hurriedly and secretively Amy of \"Parks and Recreation\" What a pitcher might have after a long game Suffix with kitchen Not keep a secret \"Oh, also ...,\" in a text Den Lent support to More like tired eyes Egyptian \"boy king\" Complete a double play, in baseball slang Less than perfect [Shame on you!] Pirate's plunder Shaggy beasts of 53-Across Follow closely, as a spy might a mark Hot dog holders Strengthens, with \"up\" Mattress giant Like many missed field goals The Phantom in \"The Phantom of the Opera\" Places where rouge goes \"___ sells\" (advertising catchphrase) Crash into from the back Betrays, in a way Greeting in Rio \"Oh yeah? ___ who?\" Flavorful Drew up, as plans Beats by ___ (audio brand) Brand of foam darts Watch chain Strikes (out) \"Drat!\" \"Them's fightin' words!\" Loving term for one caring for a sick child Rita of \"West Side Story\" High heels and others ___ vincit amor World Cup cheer Last line of a spreadsheet (as suggested by the circled squares?) ___ chicken (Indian dish) \"Well, aren't ___ pair!\" Letter addenda, for short Certain lap dog, familiarly Group that meets on the slopes Narrow waterways Synthetic fabric Horse's disapproving vote? Adamant refusal Enlivening, with \"up\" Color manually \"In that case, sure\" Hard thing to break Fasten ___ and flow The East, to the West \"Hamilton\" writer Lin-Manuel ___ Invite to one's penthouse, say Muscles that are targets of planking, informally Peon TV journalist Curry Car opposite the locomotive Funny Bombeck Late, as in making payments Make well Et ___ (and others: Lat.) \"Black-ish\" network Gumbo vegetable Coll.-level classes Decorated anew What's left of a cigarette Kind of roast \"Funny Girl\" role for which Barbra Streisand won an Oscar Network with an eye logo Many craft brews, for short Pricey seating option  GEXTá º                                                                                                                                                                              ")

(defvar puz-mode-header-spec
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

(defvar puz-mode-encoding "ISO-8859-1"
  "Encoding of body of puzzle.")

(defvar puz-mode-current_position 0
  "Position within the puz content.")

;;; using this to reset? not sure if this is right
(setq puz-mode-current_position 0)

(defvar puz-mode-header nil
  "Contents of header.")

(defvar puz-mode-solution nil
  "Contents of solution.")

(defvar puz-mode-fill nil
  "Contents of solution.")

(defvar puz-mode-height nil
  "Height of puzzle.")

(defvar puz-mode-width nil
  "Width of puzzle.")

(defvar puz-mode-solution-spec nil
  "Spec for unpacking the solution.  Also used for the fill (same size).")

;;; read the header and advance the pointer
(setq puz-mode-header (bindat-unpack puz-mode-header-spec puz-mode-content))
(setq puz-mode-current_position (bindat-length puz-mode-header-spec puz-mode-header))

(setq puz-mode-height (cdr (assoc 'height puz-mode-header)))
(setq puz-mode-width (cdr (assoc 'width puz-mode-header)))

;;; read the solution and advance the pointer
;;; TODO: combine read and advance into single method
(setq puz-mode-solution-spec (list (list 'raw 'str (* puz-mode-height puz-mode-width))))
(setq puz-mode-solution (bindat-unpack puz-mode-solution-spec puz-mode-content puz-mode-current_position))
(setq puz-mode-current_position (+ puz-mode-current_position (* puz-mode-height puz-mode-width )))
(setq puz-mode-fill (bindat-unpack puz-mode-solution-spec puz-mode-content puz-mode-current_position))
(setq puz-mode-current_position (+ puz-mode-current_position (* puz-mode-height puz-mode-width )))

;;; (print puz-mode-current_position)
;;; (print (substring puz-mode-content puz-mode-current_position (+ 32 puz-mode-current_position)))
;;; (print (eq (aref (substring puz-mode-content (+ 31 puz-mode-current_position) (+ 32 puz-mode-current_position)) 0) 0))

(defun puz-mode-read_string ()
  "Read until null character starting from puz-mode-current-position."
  (let ((next_null_pos puz-mode-current_position)
	(string_so_far ""))
    (while (not (eq (aref (substring puz-mode-content next_null_pos (+ 1 next_null_pos)) 0) 0))
      (setq string_so_far (concat string_so_far (substring puz-mode-content next_null_pos (+ 1 next_null_pos))))
      (setq next_null_pos (+ next_null_pos 1))
      )
    (setq puz-mode-current_position (+ next_null_pos 1))
    string_so_far
    ))

;;; next up are title, author and copyright.
;;; then clues. each of these is a null-terminated string of variable length.

(defvar puz-mode-title ""
  "Title of puzzle.")

(defvar puz-mode-author ""
  "Author of puzzle.")

(defvar puz-mode-copyright ""
  "Copyright of puzzle.")

(setq puz-mode-title (puz-mode-read_string))
(setq puz-mode-author (puz-mode-read_string))
(setq puz-mode-copyright (puz-mode-read_string))

(print puz-mode-copyright)

;;; Read the clues now

(defvar puz-mode-clues nil
  "List of all the clues.")

(setq puz-mode-clues nil)

(let ((numclues (cdr (assoc 'numclues puz-mode-header))))
  (dotimes (i numclues)
    (setq puz-mode-clues (cons (puz-mode-read_string) puz-mode-clues))))
(setq puz-mode-clues (nreverse puz-mode-clues))

(print (nth 78 puz-mode-clues))
;;; there's some more stuff but this is enough for now.




(provide 'puz-sample)
;;; puz-sample.el ends here
