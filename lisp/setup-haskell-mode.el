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

(provide 'setup-haskell-mode)
