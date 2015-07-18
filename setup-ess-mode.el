(use-package ess
  :ensure t
  :mode ("\\.[rR]\\'" . R-mode)
  :config
  (setq ess-ask-for-ess-directory nil
        ess-eval-visibly nil))

(provide 'setup-ess-mode)
