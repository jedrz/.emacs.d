(use-package ess
  :ensure t
  :mode ("\\.[rR]\\'" . R-mode)
  :init
  (add-hook 'ess-mode-hook (lambda () (run-hooks #'prog-mode-hook)))
  :config
  (setq ess-ask-for-ess-directory nil
        ess-eval-visibly nil
        ;; Keep global .Rhistory file.
        ess-history-directory "~/.R/"))

(provide 'setup-ess-mode)
