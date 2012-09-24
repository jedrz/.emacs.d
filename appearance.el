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
          (not (find (car custom-enabled-themes) my-themes)))
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
(global-linum-mode 1)

;; Show column number in status bar
(column-number-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Indicate empty lines after the buffer end
(setq-default indicate-empty-lines t)

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
