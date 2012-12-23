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

(defun isearch-forward-use-region (beg end)
  "Search forward for active region."
  (interactive "r")
  (isearch-use-region 'isearch-forward beg end))

(defun isearch-backward-use-region (beg end)
  "Search backward for active region."
  (interactive "r")
  (isearch-use-region 'isearch-backward beg end))

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
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

(defun sudo-edit (&optional arg)
  (interactive "P")
  (if (and arg buffer-file-name)
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))
    (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))))

(defun google-search ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Google: "))))))

(defun view-url ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)))

(defun run-urxvt-with-current-dir ()
  "Run urxvt and change directory in terminal to the current one."
  (interactive)
  (let ((urxvt-process (start-process "urxvt-process" nil "urxvt")))
    (process-send-string urxvt-process (concat "cd " default-directory))))
