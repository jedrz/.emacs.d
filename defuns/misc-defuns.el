;;; Misc defuns

(defun isearch-use-region (isearch-function beg end)
  "Call interactively `isearch-function' and use active region as saarch string.
If there is not active region then call only `isearch-function'"
  (if (region-active-p)
   (let ((selection (buffer-substring-no-properties beg end)))
     (deactivate-mark)
     (call-interactively isearch-function)
     (isearch-yank-string selection))
   (call-interactively isearch-function)))

;;;###autoload
(defun isearch-forward-use-region (beg end)
  "Search forward for active region."
  (interactive "r")
  (isearch-use-region 'isearch-forward beg end))

;;;###autoload
(defun isearch-backward-use-region (beg end)
  "Search backward for active region."
  (interactive "r")
  (isearch-use-region 'isearch-backward beg end))

;;;###autoload
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (call-interactively 'goto-line))
    (linum-mode -1)))

;; Add spaces and proper formatting to linum-mode. It uses more room than
;; necessary, but that's not a problem since it's only in use when going to
;; lines.
(setq linum-format
      (lambda (line)
        (propertize
         (format (concat " %"
                         (number-to-string
                          (length (number-to-string
                                   (line-number-at-pos (point-max)))))
                         "d ")
                 line)
         'face 'linum)))

;;;###autoload
(defun sudo-edit (&optional arg)
  (interactive "P")
  (if (and arg buffer-file-name)
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))
    (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))))

;;;###autoload
(defun google-search ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Google: "))))))

;;;###autoload
(defun view-url ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)))

;;;###autoload
(defun ispell-cycle-dicts ()
  "Switch between `ispell-my-dicts' dictionaries."
  (interactive)
  (ispell-change-dictionary (nth
                             (% (1+ (position ispell-current-dictionary
                                              ispell-my-dicts
                                              :test 'string=))
                                (length ispell-my-dicts))
                             ispell-my-dicts)))

;;;###autoload
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

;;;###autoload
(defun run-urxvt-with-current-dir ()
  "Run urxvt and change directory in terminal to the current one."
  (interactive)
  (let ((urxvt-process (start-process "urxvt-process" nil "urxvt")))
    (process-send-string urxvt-process (concat "cd " default-directory))))
