;;; Misc defuns

(eval-when-compile (require 'cl))       ; for position

(defun isearch-use-region (isearch-function)
  "Call interactively `isearch-function' and use active region as search string.
If there is no active region then just call `isearch-function'."
  (if (region-active-p)
      (let ((selection (buffer-substring-no-properties
                        (region-beginning) (region-end))))
        (deactivate-mark)
        (call-interactively isearch-function)
        (isearch-yank-string selection))
    (call-interactively isearch-function)))

;;;###autoload
(defun isearch-forward-use-region ()
  "Search forward for active region or input."
  (interactive)
  (isearch-use-region 'isearch-forward))

;;;###autoload
(defun isearch-backward-use-region ()
  "Search backward for active region or input."
  (interactive)
  (isearch-use-region 'isearch-backward))

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
(defun hippie-expand-lines ()
  "Try to expand entire line."
  (interactive)
  (let ((hippie-expand-try-functions-list '(try-expand-line
                                            try-expand-line-all-buffers)))
    (hippie-expand nil)))

;;;###autoload
(defun sudo-edit (&optional arg)
  (interactive "P")
  (if (and arg buffer-file-name)
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))
    (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))))

;;;###autoload
(defun byte-recompile-emacs-directory ()
  "Recompile my files."
  (interactive)
  ;; Be quiet about compilation.
  (let (font-lock-verbose
        byte-compile-verbose
        (directories-to-recompile '("lisp" "defuns")))
    (mapc (lambda (dir)
            (byte-recompile-directory
             (concat user-emacs-directory dir)
             0))
          directories-to-recompile)))

;;;###autoload
(defun byte-recompile-full-emacs-directory ()
  "Recompile all files in `user-emacs-directory'."
  (interactive)
  ;; Be quiet about compilation.
  (let (font-lock-verbose
        byte-compile-verbose)
    (byte-recompile-directory user-emacs-directory 0)))

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
(defun run-terminal-with-current-dir ()
  "Run terminal and change directory in terminal to the current one."
  (interactive)
  (let ((terminal-process (start-process "terminal-process" nil "urxvt")))
    (process-send-string terminal-process (concat "cd " default-directory))))

;; http://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html
;;;###autoload
(defun ispell-word-then-abbrev (p)
  "Call `ispell-word'. Then create an abbrev for the correction made.
With prefix P, create local abbrev. Otherwise it will be global."
  (interactive "P")
  (let ((bef (downcase (or (thing-at-point 'word) ""))) aft)
    (call-interactively 'ispell-word)
    (setq aft (downcase (or (thing-at-point 'word) "")))
    (unless (string= aft bef)
      (message "\"%s\" now expands to \"%s\" %sally"
               bef aft (if p "loc" "glob"))
      (define-abbrev
        (if p local-abbrev-table global-abbrev-table)
        bef aft))))

;;https://oleksandrmanzyuk.wordpress.com/2011/10/23/a-persistent-command-history-in-emacs/

;;;###autoload
(defun comint-write-history-on-exit (process event)
  (comint-write-input-ring)
  (let ((buf (process-buffer process)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (insert (format "\nProcess %s %s" process event))))))

;;;###autoload
(defun turn-on-comint-history ()
  (let ((process (get-buffer-process (current-buffer))))
    (when process
      (setq comint-input-ring-file-name
            (format (concat user-emacs-directory "inferior-%s-history")
                    (process-name process)))
      (comint-read-input-ring)
      (set-process-sentinel process
                            #'comint-write-history-on-exit))))

;;;###autoload
(defun comint-write-input-ring-all-buffers ()
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (comint-write-input-ring)))
        (buffer-list)))
