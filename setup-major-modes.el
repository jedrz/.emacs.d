(use-package markdown-mode
  :ensure t
  :defer t)

(use-package yaml-mode
  :ensure t
  :defer t)

(add-to-list 'auto-mode-alist '("\\.zsh$" . sh-mode))

(use-package pkgbuild-mode
  :ensure t
  :defer t)

(use-package scala-mode2
  :ensure t
  :defer t)

(provide 'setup-major-modes)
