;;; Common settings for modes deriving from prog-mode

(defun add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

(defun prog-mode-defaults ()
  "Default coding hook"
  (flyspell-prog-mode)
  (turn-on-auto-fill)
  (local-comment-auto-fill)
  (add-watchwords)
  (local-set-key (kbd "RET") 'reindent-then-newline-and-indent))

(add-hook 'prog-mode-hook 'prog-mode-defaults)

(provide 'setup-prog-mode)
