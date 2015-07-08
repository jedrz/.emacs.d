(after 'racer
  (setq racer-rust-src-path (expand-file-name "~/.rustc-1.1.0/src")))

(add-hook 'rust-mode-hook
          (lambda ()
            ;; racer-activate disable auto completion after delay. With this
            ;; option enabled completion doesn't work.
            (racer-activate)
            (local-set-key (kbd "M-.") 'racer-find-definition)))

(add-hook 'flycheck-mode-hook 'flycheck-rust-setup)

(provide 'setup-rust-mode)
