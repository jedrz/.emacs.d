;;; Defuns for editing text

;; Some functions from http://github.com/magnars/.emacs.d/blob/master/defuns/editing-defuns.el

;;;###autoload
(defun back-to-indentation-or-beginning ()
  "Move point to beginning of line only if looking at indentation."
  (interactive)
  (if (and (looking-back "^[[:blank:]]+")
           (not (looking-at "[[:blank:]]+")))
      (beginning-of-line)
    (back-to-indentation)))

;;;###autoload
(defun new-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

;;;###autoload
(defun new-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

;;;###autoload
(defun new-line-in-between ()
  (interactive)
  (newline)
  (save-excursion
    (newline)
    (indent-for-tab-command))
  (indent-for-tab-command))

;;;###autoload
(defun empty-line-below (arg)
  "Insert empty line below point.
If ARG is positive then ARG lines are inserted below otherwise above."
  (interactive "p")
  (save-excursion
    (if (< arg 0)
        (dotimes (_ (- arg))
          (beginning-of-line)
          (newline))
      (forward-line)
      (newline arg)))
  ;; If point was at beginning of line with negative prefix argument
  ;; then the point is `arg' lines too high.
  ;; Old position has to be restored manually.
  (when (and (< arg 0) (looking-at "^"))
    (forward-line (- arg))))

;;;###autoload
(defun empty-line-above (arg)
  "Insert empty line above point.
If ARG is positive then ARG lines are inserted above otherwise below."
  (interactive "p")
  (empty-line-below (- arg)))

;;;###autoload
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated."
  (interactive "p")
  (save-excursion
    (if (region-active-p)
        (duplicate-region arg)
      (duplicate-current-line arg))))

;;;###autoload
(defun duplicate-region (num &optional start end)
  "Duplicates the region bounded by START and END NUM times.
If no START and END is provided, the current region-beginning and
region-end is used. Adds the duplicated text to the kill ring."
  (interactive "p")
  (let* ((start (or start (region-beginning)))
         (end (or end (region-end)))
         (region (buffer-substring start end)))
    (kill-ring-save start end)
    (goto-char start)
    (dotimes (i num)
      (insert region))))

;;;###autoload
(defun duplicate-current-line (num)
  "Duplicate the current line NUM times."
  (interactive "p")
  (when (eq (point-at-eol) (point-max))
    (goto-char (point-max))
    (newline)
    (forward-char -1))
  (duplicate-region num (point-at-bol) (1+ (point-at-eol))))

;;;###autoload
(defun yank-and-indent ()
  (interactive)
  (let ((start (point)))
    (yank)
    (indent-region start (point))))

;;;###autoload
(defun yank-pop-and-indent (arg)
  (interactive "p")
  (let ((start (point)))
    (yank-pop arg)
    (indent-region start (point))))

;;;###autoload
(defun kill-to-beginning-of-line ()
  (interactive)
  (kill-region (save-excursion (beginning-of-line) (point))
               (point)))

;;;###autoload
(defun copy-to-end-of-line ()
  (interactive)
  (kill-ring-save (point)
                  (line-end-position))
  (message "Copied to end of line"))

;;;###autoload
(defun copy-whole-lines (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (1+ arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

;;;###autoload
(defun copy-line (arg)
  "Copy to end of line, or as many lines as prefix argument"
  (interactive "P")
  (if (null arg)
      (copy-to-end-of-line)
    (copy-whole-lines (prefix-numeric-value arg))))

;;;###autoload
(defun save-region-or-current-line (arg)
  (interactive "P")
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (copy-line arg)))

;;;###autoload
(defun kill-and-retry-line ()
  "Kill the entire current line and reposition point at indentation."
  (interactive)
  (back-to-indentation)
  (kill-line))

;;;###autoload
(defun comment-kill-all ()
  "Kill all comments in buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (comment-kill (save-excursion
                    (goto-char (point-max))
                    (line-number-at-pos)))))

;;;###autoload
(defun comment-or-uncomment-current-line-or-region ()
  "Comment or uncomment current line or region."
  (interactive)
  (if (region-active-p)
      (call-interactively 'comment-or-uncomment-region)
    (comment-or-uncomment-region (line-beginning-position)
                                 (line-end-position))))

;;;###autoload
(defun camelize-buffer ()
  (interactive)
  (goto-char 0)
  (ignore-errors
    (replace-next-underscore-with-camel 0))
  (goto-char 0))

;;;###autoload
(defun replace-next-underscore-with-camel (arg)
  (interactive "p")
  (if (> arg 0)
      (setq arg (1+ arg))) ; 1-based index to get eternal loop with 0
  (let ((case-fold-search nil))
    (while (not (= arg 1))
      (search-forward-regexp "\\b_[a-z]")
      (forward-char -2)
      (delete-char 1)
      (capitalize-word 1)
      (setq arg (1- arg)))))

(defun point-in-string-p ()
  (nth 3 (syntax-ppss)))

(defun point-in-comment-p ()
  (nth 4 (syntax-ppss)))

(defun move-point-forward-out-of-string ()
  (while (point-in-string-p) (forward-char)))

(defun move-point-backward-out-of-string ()
  (while (point-in-string-p) (backward-char)))

(defun move-forward-out-of-param ()
  (while (not (looking-at ")\\|, \\| ?}\\| ?\\]"))
    (cond
     ((point-in-string-p) (move-point-forward-out-of-string))
     ((looking-at "(\\|{\\|\\[") (forward-list))
     (t (forward-char)))))

(defun move-backward-out-of-param ()
  (while (not (looking-back "(\\|, \\|{ ?\\|\\[ ?"))
    (cond
     ((point-in-string-p) (move-point-backward-out-of-string))
     ((looking-back ")\\|}\\|\\]") (backward-list))
     (t (backward-char)))))

;;;###autoload
(defun transpose-params ()
  "Presumes that params are in the form (p, p, p) or {p, p, p} or [p, p, p]"
  (interactive)
  (let* ((end-of-first (cond
                        ((looking-at ", ") (point))
                        ((and (looking-back ",") (looking-at " ")) (- (point) 1))
                        ((looking-back ", ") (- (point) 2))
                        (t (error "Place point between params to transpose."))))
         (start-of-first (save-excursion
                           (goto-char end-of-first)
                           (move-backward-out-of-param)
                           (point)))
         (start-of-last (+ end-of-first 2))
         (end-of-last (save-excursion
                        (goto-char start-of-last)
                        (move-forward-out-of-param)
                        (point))))
    (transpose-regions start-of-first end-of-first start-of-last end-of-last)))

;;;###autoload
(defun fill-paragraph-or-indent ()
  "Fill paragraph or indent code.
If point is in string or comment or current major mode is not a
prog-mode, text is filled. Otherwise a region or defun is indented."
  (interactive)
  (if (or (point-in-string-p)
          (point-in-comment-p)
          (not (derived-mode-p 'prog-mode)))
      (call-interactively 'fill-paragraph)
    (apply 'indent-region (if (region-active-p)
                              (list (region-beginning) (region-end))
                            (list (save-excursion (beginning-of-defun) (point))
                                  (save-excursion (end-of-defun) (point)))))))

;;;###autoload
(defun unfill-paragraph ()
  "Unfill paragraph or region."
  (interactive)
  (let ((fill-column (point-max)))
    (call-interactively 'fill-paragraph)))

;;;###autoload
(defun copy-rectangle (start end)
  "Save rectangle as the last killed one."
  (interactive "r")
  (setq killed-rectangle (extract-rectangle start end)))
