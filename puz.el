;;; package -- Summary

;;; Commentary:

;;; Code:

(defvar puz_mode-content ",AACROSS&DOWN êK3ÝÝ«~Ä1.3      nytimes.comN    APSE..BLAB..TUTBOOTY.TAIL.BUNSSERTA.WIDE.ERIKCHEEKS.REAREND.OLA.SEZ.DRAFTEDNERF.XES.ITSWARDRMOM.SHOES.OLE...BOTTOMROW...PSS.RAYON.NEIGHOKTHEN.TIE.ASIAMIRANDA.ABS.ANN.CABOOSE.BEHINDALII.OKRA.REDIDBUTT.RUMP.FANNYCBS..IPAS..LOGE----..----..--------.----.---------.----.----------.-------.---.---.-----------.---.-----------.-----.---...---------...---.-----.-----------.---.-----------.---.---.-------.----------.----.---------.----.--------..----..----NY Times, Monday, May 11, 2020  Ross Trudeau / Will Shortz © 2020, The New York Times Chapel recess Leave hurriedly and secretively Amy of \"Parks and Recreation\" What a pitcher might have after a long game Suffix with kitchen Not keep a secret \"Oh, also ...,\" in a text Den Lent support to More like tired eyes Egyptian \"boy king\" Complete a double play, in baseball slang Less than perfect [Shame on you!] Pirate's plunder Shaggy beasts of 53-Across Follow closely, as a spy might a mark Hot dog holders Strengthens, with \"up\" Mattress giant Like many missed field goals The Phantom in \"The Phantom of the Opera\" Places where rouge goes \"___ sells\" (advertising catchphrase) Crash into from the back Betrays, in a way Greeting in Rio \"Oh yeah? ___ who?\" Flavorful Drew up, as plans Beats by ___ (audio brand) Brand of foam darts Watch chain Strikes (out) \"Drat!\" \"Them's fightin' words!\" Loving term for one caring for a sick child Rita of \"West Side Story\" High heels and others ___ vincit amor World Cup cheer Last line of a spreadsheet (as suggested by the circled squares?) ___ chicken (Indian dish) \"Well, aren't ___ pair!\" Letter addenda, for short Certain lap dog, familiarly Group that meets on the slopes Narrow waterways Synthetic fabric Horse's disapproving vote? Adamant refusal Enlivening, with \"up\" Color manually \"In that case, sure\" Hard thing to break Fasten ___ and flow The East, to the West \"Hamilton\" writer Lin-Manuel ___ Invite to one's penthouse, say Muscles that are targets of planking, informally Peon TV journalist Curry Car opposite the locomotive Funny Bombeck Late, as in making payments Make well Et ___ (and others: Lat.) \"Black-ish\" network Gumbo vegetable Coll.-level classes Decorated anew What's left of a cigarette Kind of roast \"Funny Girl\" role for which Barbra Streisand won an Oscar Network with an eye logo Many craft brews, for short Pricey seating option  GEXTá º                                                                                                                                                                              ")

(defvar puz_mode-header_spec
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

(defvar puz_mode-encoding "ISO-8859-1"
  "Encoding of body of puzzle.")

(defvar puz_mode-current_position 0
  "Position within the puz content.")

(defvar puz_mode-header nil
  "Contents of header.")

(defvar puz_mode-solution nil
  "Contents of solution.")

(defvar puz_mode-fill nil
  "Contents of solution.")

(defvar puz_mode-height nil
  "Height of puzzle.")

(defvar puz_mode-width nil
  "Width of puzzle.")

(defvar puz_mode-solution_spec nil
  "Spec for unpacking the solution.  Also used for the fill (same size).")

;;; read the header and advance the pointer
(setq puz_mode-header (bindat-unpack puz_mode-header_spec puz_mode-content))
(setq puz_mode-current_position (bindat-length puz_mode-header_spec puz_mode-header))

(setq puz_mode-height (cdr (assoc 'height puz_mode-header)))
(setq puz_mode-width (cdr (assoc 'width puz_mode-header)))

;;; read the solution and advance the pointer
;;; TODO: combine read and advance into single method
(setq puz_mode-solution_spec (list (list 'raw 'str (* puz_mode-height puz_mode-width))))
(setq puz_mode-solution (bindat-unpack puz_mode-solution_spec puz_mode-content puz_mode-current_position))
(setq puz_mode-current_position (+ puz_mode-current_position (* puz_mode-height puz_mode-width )))
(setq puz_mode-fill (bindat-unpack puz_mode-solution_spec puz_mode-content puz_mode-current_position))

;;;(setq puz_mode-solution (buffer-substring puz_mode-content puz_mode-current_position (+ puz_mode-current_position (* puz_mode-height puz_mode-width))))


(provide 'puz)
;;; puz.el ends here
