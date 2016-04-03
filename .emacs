;; Prevent the cursor from blinking
(blink-cursor-mode 0)
;; Don't use messages that you don't read
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
;; Don't let Emacs hurt your ears
(setq visible-bell t)

;; You need to set `inhibit-startup-echo-area-message' from the
;; customization interface:
;; M-x customize-variable RET inhibit-startup-echo-area-message RET
;; then enter your username
(setq inhibit-startup-echo-area-message "markmliu")
;; Who use the bar to scroll?
(scroll-bar-mode 0)
(tool-bar-mode 0)
;; (menu-bar-mode 0)
(custom-set-faces
  '(default ((t (:background "black" :foreground "grey"))))
  '(fringe ((t (:background "black")))))

;; make font smaller
(set-face-attribute 'default (selected-frame) :height 100)

;; start off maximized
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; default window splitting side-by-side
(setq split-height-threshold nil)
(setq split-width-threshold 0)

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(define-key ctl-x-4-map "t" 'toggle-window-split)


;; toggle window split
(global-set-key (kbd "C-|") 'toggle-window-split)

;; default to even horizontal split
(defadvice split-window-horizontally (after rebalance-windows activate)
  (balance-windows))
(ad-activate 'split-window-horizontally)

;; load from ~/.emacs.d/lisp/
(load "~/.emacs.d/lisp/minimap.el")
(load "~/.emacs.d/lisp/google.el")
;; use shift + arrow to navigate windows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; shared copy paste
(setq x-select-enable-clipboard t)

;; recentf stuff
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
