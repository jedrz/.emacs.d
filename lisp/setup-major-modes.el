(use-package markdown-mode
  :ensure t
  :defer t)

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package pkgbuild-mode
  :ensure t
  :defer t)

(use-package scala-mode
  :ensure t
  :defer t)

(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package kotlin-mode
  :ensure t
  :defer t)

(use-package elm-mode
  :ensure t
  :defer t)

(use-package terraform-mode
  :ensure t
  :defer t
  :init
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

(use-package company-terraform
  :ensure t
  :defer t
  :init
  (add-hook 'terraform-mode-hook #'company-terraform-init))

(use-package groovy-mode
  :ensure t
  :defer t)

(provide 'setup-major-modes)
