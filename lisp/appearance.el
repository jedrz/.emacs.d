;; Fira Code font but no ligatures on Emacs unfortunalety :(
(when (window-system)
  (set-default-font "Fira Code-11"))

(use-package flatui-theme
  :ensure t
  :config
  (load-theme 'flatui t))

;; Show column number in status bar.
(column-number-mode 1)

;; Highlight current line.
(global-hl-line-mode 1)

;; Visualize some kinds of blank.
(use-package whitespace
  :defer t
  :diminish whitespace-mode
  :init
  (global-whitespace-mode 1)
  :config
  (setq whitespace-style '(face trailing)))

;; Indicate empty lines after the buffer end.
(setq-default indicate-empty-lines t)

;; Highlight matching parens.
(use-package paren
  :defer t
  :init
  (show-paren-mode 1)
  :config
  (setq show-paren-delay 0))

;; Turn off blinking.
(blink-cursor-mode -1)

;; Hide tool bar.
(tool-bar-mode -1)

;; Hide scroll bars.
(scroll-bar-mode -1)

;; Hide menu bar.
(menu-bar-mode -1)

;; Start maximized.
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Smooth scrolling.
(setq scroll-conservatively 10000
      scroll-margin 3
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

;; More useful frame title.
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))
        " - " invocation-name))

;; Unique buffer names.
(use-package uniquify
  :defer t
  :config
  (progn
    (require 'uniquify)
    (setq uniquify-buffer-name-style 'forward)))

;; Smart mode line.
(use-package smart-mode-line
  :ensure t
  :defer t
  :init
  (sml/setup))

;; fill-column-indicator
;; Enable only in text- and prog-modes.
(use-package fill-column-indicator
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook #'fci-mode)
    (add-hook 'text-mode-hook #'fci-mode))
  :config
  (progn
    ;; Workarounds for popup library.

    (defadvice popup-create (before suppress-fci-mode activate compile)
      "Suspend fci-mode while popups are visible"
      (when fci-mode
        (turn-off-fci-mode)))

    (defadvice popup-delete (after restore-fci-mode activate compile)
      "Restore fci-mode when all popups have closed"
      (unless fci-mode
        (turn-on-fci-mode)))))

(provide 'appearance)