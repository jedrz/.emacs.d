(define-derived-mode mips-mode asm-mode "MIPS"
  "Major mode for editing MIPS assembler code."
  ;; Unset ; key.
  (local-unset-key (vector asm-comment-char))
  (set (make-local-variable 'asm-comment-char) ?#)
  (local-set-key (vector asm-comment-char) 'asm-comment)
  ;; Update syntax for new comment char.
  (set-syntax-table (make-syntax-table asm-mode-syntax-table))
  (modify-syntax-entry asm-comment-char "< b")
  ;; Fix one level comments.
  (set (make-local-variable 'comment-start) (string asm-comment-char)))

(defun asm-align (&optional column)
  "Align asm instructions.

Move text after every instruction to COLUMN or 12 if nil
and fix inline comments by indenting to `comment-column'."
  (interactive "P")
  (setq column (if column
                   (prefix-numeric-value column)
                 12))
  (save-excursion
    ;; Align instructions.
    (goto-char (point-min))
    (while (search-forward-regexp "^[ \t]+[[:alnum:]]+" nil t)
      (delete-horizontal-space)
      (indent-to-column column)
      ;; Delete white space after instructions without arguments.
      (when (eolp)
        (delete-horizontal-space))
      ;; Fix inline comments.
      (when (search-forward (format "%c" asm-comment-char)
                            (line-end-position)
                            t)
        (indent-for-comment)))))

(defun asm-convert-comment-chars-to-single ()
  "Convert multiple comment chars to only single one."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp (format "%c\\{2,3\\}" asm-comment-char)
                                  nil
                                  t)
      (replace-match (char-to-string asm-comment-char)))))

(provide 'setup-asm-mode)
