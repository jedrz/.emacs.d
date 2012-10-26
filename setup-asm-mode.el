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

(defun asm-align (ins-column)
  "Align asm instructions and comments.

Move text after every instruction to INS-COLUMN column
and align inline comments to `comment-column'."
  (interactive "p")
  (save-excursion
    ;; Align instructions.
    (goto-char (point-min))
    (while (search-forward-regexp "^[ \t]+[^#:\.a-z]+[a-z]+" nil t)
      (indent-to-column ins-column)
      ;; Delete white space after instructions without arguments.
      (when (eolp)
       (delete-horizontal-space)))
    ;; Align inline comments.
    (goto-char (point-min))
    (while (search-forward-regexp (format " %c\\{1\\} " asm-comment-char) nil t)
      (indent-for-comment))))

(provide 'setup-asm-mode)
