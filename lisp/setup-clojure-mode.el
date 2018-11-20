(use-package clojure-mode
  :ensure t
  :defer t
  :config
  (with-eval-after-load 'smartparens
    (add-hook 'clojure-mode-hook 'smartparens-strict-mode)))

(use-package cider
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'clojure-mode
    (add-hook 'clojure-mode-hook 'cider-mode))
  :config
  (progn
    (setq nrepl-hide-special-buffers t
          cider-repl-pop-to-buffer-on-connect nil
          cider-repl-use-clojure-font-lock t)

    (with-eval-after-load 'smartparens
      (add-hook 'cider-repl-mode-hook #'smartparens-strict-mode))))

(use-package clj-refactor
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'clojure-mode
    (add-hook 'clojure-mode-hook
              (lambda ()
                (clj-refactor-mode 1)
                (cljr-add-keybindings-with-prefix "C-c r")))))

(provide 'setup-clojure-mode)
