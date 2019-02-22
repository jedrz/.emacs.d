(use-package ess
  :ensure t
  :mode ("\\.[rR]\\'" . R-mode)
  :init
  (add-hook 'ess-mode-hook (lambda () (run-hooks #'prog-mode-hook)))
  :config
  (progn
    (setq ess-ask-for-ess-directory nil
          ess-eval-visibly nil
          ;; Keep global .Rhistory file.
          ess-history-directory "~/.R/")
    (with-eval-after-load 'flycheck
      (setq flycheck-lintr-linters "with_defaults(camel_case_linter = NULL, snake_case_linter, object_usage_linter = NULL)"))))

(provide 'setup-ess-mode)
