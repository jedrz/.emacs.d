(add-hook 'clojure-mode-hook 'smartparens-strict-mode)

;; Cider
(add-hook 'clojure-mode-hook 'cider-mode)
(after 'cider
  (setq nrepl-hide-special-buffers t
        cider-repl-pop-to-buffer-on-connect nil
        cider-repl-use-clojure-font-lock t)

  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

  (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode))

;; clj refactor
(add-hook 'clojure-mode-hook
          (lambda ()
            (clj-refactor-mode 1)
            (cljr-add-keybindings-with-prefix "C-c r")))

(provide 'setup-clojure-mode)
