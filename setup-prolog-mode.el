(setq-default prolog-system 'swi)

(unless (fboundp 'switch-to-prolog)
  (defun switch-to-prolog (arg)
    (interactive "P")
    (run-prolog arg)
    (switch-to-buffer "*prolog*")))

(provide 'setup-prolog-mode)
