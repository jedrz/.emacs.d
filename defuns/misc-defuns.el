;;; Misc defuns

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

(defun mips-mode ()
  "Run asm mode with adjusted defaults for MIPS."
  (interactive)
  (setq asm-comment-char ?#)
  (asm-mode))
