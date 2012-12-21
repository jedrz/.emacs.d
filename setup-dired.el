(require 'dired+)

(setq dired-listing-switches "-al --group-directories-first"
      dired-dwim-target t               ; Copy to the second visible dired buffer
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
      (dired-sort-not-hidden-first-recursive (line-number-at-pos))
    (set-buffer-modified-p nil))))

(defun dired-sort-not-hidden-first-recursive (insert-line-number)
  ;; Search backward for first hidden file.
  (when (search-backward-regexp " +\\."
                                (save-excursion
                                  (goto-char (point-min))
                                  (search-forward ".." nil t))
                                t)
    (let ((line (buffer-substring (line-beginning-position)
                                  (1+ (line-end-position)))))
      (dired-kill-line)
      ;; Paste the line at `insert-line-number'.
      (goto-char (point-min))
      (forward-line (1- insert-line-number))
      (insert line))
    ;; Process previous line (just before the inserted one).
    (forward-line -2)
    (end-of-line)
    (dired-sort-not-hidden-first-recursive (line-number-at-pos))))

;; Simple hook doesn't work.
(defadvice dired-readin (after dired-after-updating-hook activate)
  "Sort dired listings with not hidden files first."
  (dired-sort-not-hidden-first))

(eval-after-load "wdired"
  '(progn
     (define-key wdired-mode-map [remap beginning-of-buffer] 'dired-goto-top)
     (define-key wdired-mode-map [remap end-of-buffer] 'dired-goto-bottom)))

(provide 'setup-dired)
