(require 'dired+)

(setq dired-listing-switches "-al --group-directories-first"
      dired-dwim-target t               ; Copy to the second visible dired buffer
      dired-auto-revert-buffer t        ; Revert buffer on revisiting
      dired-recursive-copies 'always)

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

(defun dired-sort-not-hidden-first ()
  "Sort files not starting with dot first."
  (save-excursion
    (let (buffer-read-only)
      ;; Go to the end of last line.
      (goto-char (point-max))
      (backward-char)
      (let ((current-insert-line-number (line-number-at-pos)))
        ;; Search backward for first hidden file.
        (while (search-backward-regexp " +\\."
                                       (save-excursion
                                         (goto-char (point-min))
                                         (search-forward ".." nil t))
                                       t)
          (let ((line (buffer-substring (line-beginning-position)
                                        (1+ (line-end-position)))))
            (dired-kill-line)
            ;; Paste the line at `current-insert-line-number'.
            (goto-char (point-min))
            (forward-line (1- current-insert-line-number))
            (insert line))
          ;; Process previous line (just before the inserted one).
          (forward-line -2)
          (end-of-line)
          (setq current-insert-line-number (line-number-at-pos))))
      (set-buffer-modified-p nil))))

;; Simple hook doesn't work.
(defadvice dired-readin (after dired-after-updating-hook activate compile)
  "Sort dired listings with not hidden files first."
  (dired-sort-not-hidden-first))

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
