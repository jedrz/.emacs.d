(use-package python
  :defer t
  :config
  (progn
    (add-hook 'inferior-python-mode-hook #'turn-on-comint-history)))

(use-package virtualenvwrapper
  :ensure t
  :defer t)

(use-package lsp-pyright
  :ensure t
  :defer t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))

(provide 'setup-python-mode)
