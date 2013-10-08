(require 'dired+)
(require 'dired-details)

(setq dired-listing-switches "-alhv --group-directories-first"
      dired-dwim-target t               ; Copy to the second visible dired buffer
      dired-auto-revert-buffer t        ; Revert buffer on revisiting
      dired-recursive-copies 'always)

;; Hide usually redundant file details
(dired-details-install)
(setq-default dired-details-hidden-string "--- ")

(defun dired-goto-top ()
  "Move to the third line (..)."
  (interactive)
  (beginning-of-buffer)
  (search-forward ".." nil nil)
  (dired-move-to-filename))

(define-key dired-mode-map [remap beginning-of-buffer] 'dired-goto-top)

(defun dired-goto-bottom ()
  "Move to the last file."
  (interactive)
  (end-of-buffer)
  (forward-line -1)
  (dired-move-to-filename))

(define-key dired-mode-map [remap end-of-buffer] 'dired-goto-bottom)

(defadvice dired-clean-up-after-deletion
  (before dired-auto-kill-buffer-after-deletion activate compile)
  "Kill buffers associated with file or directory being removed.
Do not ask for permission."
  (let* ((fn (ad-get-arg 0))
         (buf (get-file-buffer fn))
         (buf-list (dired-buffers-for-dir (expand-file-name fn))))
    (and buf (kill-buffer buf))
    (mapc 'kill-buffer buf-list)))

(after 'wdired
  (define-key wdired-mode-map [remap beginning-of-buffer] 'dired-goto-top)
  (define-key wdired-mode-map [remap end-of-buffer] 'dired-goto-bottom))

(provide 'setup-dired)
