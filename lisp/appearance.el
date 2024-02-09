(add-to-list 'default-frame-alist '(font . "JetBrains Mono 10"))

(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))

;; Show column number in status bar.
(column-number-mode 1)

;; Highlight current line.
(global-hl-line-mode 1)

;; Visualize some kinds of blank.
(use-package whitespace
  :defer t
  :diminish global-whitespace-mode
  :init
  (global-whitespace-mode 1)
  :config
  (setq whitespace-style '(face trailing)))

;; Indicate empty lines after the buffer end.
(setq-default indicate-empty-lines t)

;; Make cursor the width of the character it is under i.e. full width of a TAB.
;; http://pragmaticemacs.com/emacs/adaptive-cursor-width/
(setq x-stretch-cursor t)

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
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Fix frame fullscreen being not completely fullscreen.
(setq frame-resize-pixelwise t)

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

;; Disable beep.
(setq visible-bell t)

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

;; Enable only in text- and prog-modes.
(use-package display-fill-column-indicator
  :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
    (add-hook 'text-mode-hook #'display-fill-column-indicator-mode)))

(use-package emojify
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'after-init-hook #'global-emojify-mode)))

(provide 'appearance)
