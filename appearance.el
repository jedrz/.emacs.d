;; Font
(set-frame-font "Inconsolata-12")
(setq default-frame-alist '((font . "Inconsolata-12")))

;; Load theme
(defvar my-themes '(sanityinc-tomorrow-night
                    sanityinc-tomorrow-day)
  "List of themes to switch between")

(load-theme (car my-themes) t)

(defun my-themes-load-next ()
  "Load next theme from `my-themes' list"
  (interactive)
  ;; Check if more than one theme is not enabled
  ;; or the theme is not from `my-themes' list
  (if (or (> (length custom-enabled-themes) 1)
          (not (find (car custom-enabled-themes) my-themes)))
      (progn
        (message "Loaded themes seem not valid. The first will be loaded.")
        ;; Disable all loaded themes
        (mapc 'disable-theme custom-enabled-themes)
        (load-theme (car my-themes)) t)
    (let* ((current-theme (car custom-enabled-themes))
           (next-theme (nth
                        (1+ (position current-theme my-themes))
                        my-themes)))
      ;; Disable currently loaded theme
      (disable-theme current-theme)
      ;; Check if current-theme wasn't the last in `my-themes'
      ;; then load the first in list
      (if next-theme
          (load-theme next-theme t)
        (load-theme (car my-themes) t))
      (message "%s loaded" (car custom-enabled-themes)))))

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

;; Hide menu bar
(menu-bar-mode -1)

;; More useful frame title
(setq frame-title-format
      '("" invocation-name ": " (:eval (if (buffer-file-name)
                                           (abbreviate-file-name (buffer-file-name))
                                         "%b"))))

;; Indicate fill-column
(define-global-minor-mode fci-global-mode
  fci-mode
  (lambda () (fci-mode 1)))
(fci-global-mode 1)

;; Set up whitespace-mode to work with fci
;; which is not compatible with show-trailing-whitespace
(setq whitespace-style '(face trailing))
(global-whitespace-mode 1)

(provide 'appearance)
