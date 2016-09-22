(add-to-list 'auto-mode-alist '("\\.zsh$" . sh-mode))

(use-package company-shell
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-shell)))

(provide 'setup-sh-mode)
