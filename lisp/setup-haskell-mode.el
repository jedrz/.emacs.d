(use-package haskell-mode
  :ensure t
  :defer t
  :hook
  (haskell-mode . interactive-haskell-mode))

(use-package lsp-haskell
  :ensure t
  :defer t
  :hook
  ((haskell-mode . lsp)
   (haskell-literate-mode . lsp)))

(use-package ormolu
  :ensure t
  :defer t
  :hook (haskell-mode . ormolu-format-on-save-mode)
  :bind
  (:map haskell-mode-map
        ("C-c r" . ormolu-format-buffer)))

(provide 'setup-haskell-mode)
