;; Font
(set-frame-font "Monaco-10.5" nil t)
;; FIXME: why the above line is not sufficient to set font with emacsclient
(setq default-frame-alist '((font . "Monaco-10.5")))

;; Load theme
(defvar my-themes '(sanityinc-tomorrow-night
                    sanityinc-tomorrow-day)
  "List of themes to switch between")

(load-theme (car my-themes) t)

(defun my-themes-cycle ()
  "Load the next theme from `my-themes' list"
  (interactive)
  ;; Check if more than one theme is enabled
  ;; or the theme is not from `my-themes' list
  (if (or (> (length custom-enabled-themes) 1)
          (not (memq (car custom-enabled-themes) my-themes)))
      (progn
        (message "Loaded themes seem not valid. The first will be loaded.")
        ;; Disable all loaded themes
        (mapc 'disable-theme custom-enabled-themes)
        (load-theme (car my-themes)) t)
    (let* ((current-theme (car custom-enabled-themes))
           ;; Choose the next theme or the first one
           (next-theme (nth
                        (% (1+ (position current-theme my-themes))
                           (length my-themes))
                        my-themes)))
      (disable-theme current-theme)
      (load-theme next-theme t)
      (message "%s loaded" (car custom-enabled-themes)))))

;; Show line numbers
;(global-linum-mode 1)

;; Show column number in status bar
(column-number-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Visualize some kinds of blank
(setq whitespace-style '(face trailing))
(global-whitespace-mode 1)

;; Indicate empty lines after the buffer end
(setq-default indicate-empty-lines t)

;; Highlight matching parens
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Turn off blinking
(blink-cursor-mode -1)

;; Remove splash screen and message
(setq inhibit-splash-screen t
      initial-scratch-message nil)

;; Hide tool bar
(tool-bar-mode -1)

;; Hide scroll bars
(scroll-bar-mode -1)

;; Hide menu bar
(menu-bar-mode -1)

;; Smooth scrolling
(setq scroll-conservatively 10000
      scroll-margin 3
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

;; More useful frame title
(setq frame-title-format
      '("" invocation-name ": " (:eval (if (buffer-file-name)
                                           (abbreviate-file-name (buffer-file-name))
                                         "%b"))))

;; Unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; fill-column-indicator
;; Enable only in text- and prog-modes
(add-hook 'prog-mode-hook 'fci-mode)
(add-hook 'text-mode-hook 'fci-mode)

;; Workaround for fci and AC's popup
(defadvice popup-create (before suppress-fci-mode activate)
  "Suspend fci-mode while popups are visible"
  (when fci-mode
    (turn-off-fci-mode)))

(defadvice popup-delete (after restore-fci-mode activate)
  "Restore fci-mode when all popups have closed"
  (when (not fci-mode)
    (turn-on-fci-mode)))

(provide 'appearance)
