;; https://github.com/serras/emacs-haskell-tutorial

(use-package haskell-mode
  :ensure t
  :defer t
  :config
  (progn
    (bind-key "C-c C-o" #'haskell-compile haskell-mode-map)
    (bind-key "C-c C-l" #'haskell-process-load-or-reload haskell-mode-map)
    (bind-key "C-c C-z" #'haskell-interactive-switch haskell-mode-map)
    (bind-key "C-c C-n C-t" #'haskell-process-do-type haskell-mode-map)
    (bind-key "C-c C-n C-i" #'haskell-process-do-info haskell-mode-map)
    (bind-key "C-c C-n C-c" #'haskell-process-cabal-build haskell-mode-map)
    (bind-key "C-c C-n c" #'haskell-process-cabal haskell-mode-map)))

(use-package hindent
  :ensure t
  :defer t
  :init
  (progn
    (setq hindent-process-path (expand-file-name "~/.local/bin/hindent"))
    (add-hook 'haskell-mode-hook #'hindent-mode)))

(provide 'setup-haskell-mode)
