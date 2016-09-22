(use-package rust-mode
  :ensure t
  :defer t)

;; Major mode for .toml Cargo files
(use-package toml-mode
  :ensure t
  :defer t)

;; Auto completion for rust
(use-package racer
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'rust-mode
    (add-hook 'rust-mode-hook
              (lambda ()
                ;; racer-activate disable auto completion after delay. With this
                ;; option enabled completion doesn't work.
                (racer-activate)
                (local-set-key (kbd "M-.") #'racer-find-definition))))
  :config
  (setq racer-rust-src-path (expand-file-name "~/.rustc-1.1.0/src")))

;; Flycheck checker for rust
(use-package flycheck-rust
  :ensure t
  :defer t
  :config
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(provide 'setup-rust-mode)
