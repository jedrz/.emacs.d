;; Font
(setq default-frame-alist '((font . "Source Code Pro-10.5")))

;; Load theme
(defvar my-themes '(sanityinc-tomorrow-night
                    sanityinc-tomorrow-day)
  "List of themes to switch between")

(load-theme (car my-themes) t)

;; Show line numbers
;(global-linum-mode 1)

;; Show column number in status bar
(column-number-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Visualize some kinds of blank
(after 'whitespace
  (setq whitespace-style '(face trailing)))
(global-whitespace-mode 1)

;; Indicate empty lines after the buffer end
(setq-default indicate-empty-lines t)

;; Highlight matching parens
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Turn off blinking
(blink-cursor-mode -1)

;; Hide tool bar
(tool-bar-mode -1)

;; Hide scroll bars
(scroll-bar-mode -1)

;; Hide menu bar
(menu-bar-mode -1)

;; Start maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Smooth scrolling
(setq scroll-conservatively 10000
      scroll-margin 3
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

;; More useful frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))
        " - " invocation-name))

;; Unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Smart mode line
(sml/setup)

;; fill-column-indicator
;; Enable only in text- and prog-modes
(add-hook 'prog-mode-hook 'fci-mode)
(add-hook 'text-mode-hook 'fci-mode)

;; Workaround for fci and AC's popup
(defadvice popup-create (before suppress-fci-mode activate compile)
  "Suspend fci-mode while popups are visible"
  (when fci-mode
    (turn-off-fci-mode)))

(defadvice popup-delete (after restore-fci-mode activate compile)
  "Restore fci-mode when all popups have closed"
  (unless fci-mode
    (turn-on-fci-mode)))

(provide 'appearance)
