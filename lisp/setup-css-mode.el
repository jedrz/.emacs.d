(use-package css-mode
  :defer t
  :config
  (setq css-indent-offset 2))

(use-package rainbow-mode
  :ensure t
  :defer t
  :init
  (add-hook 'css-mode-hook #'rainbow-mode))

(provide 'setup-css-mode)
