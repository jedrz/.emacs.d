;; Font
(set-frame-font "Inconsolata-12")
(setq default-frame-alist '((font . "Inconsolata-12")))

;; Load theme
(load-theme 'solarized-dark t)

;; Show line numbers
(global-linum-mode 1)

;; Show column number in status bar
(column-number-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Highlight matching parens
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Turn off blinking
(blink-cursor-mode -1)

;; Remove splash screen
(setq inhibit-splash-screen t)

;; Hide tool bar
(tool-bar-mode -1)

;; Hide scroll bars
(scroll-bar-mode -1)

;; Indicate fill-column
(fci-mode t)

;; Set up whitespace-mode to work with fci
;; which is not compatible with show-trailing-whitespace
(setq whitespace-style '(face trailing))
(global-whitespace-mode 1)

(provide 'appearance)
