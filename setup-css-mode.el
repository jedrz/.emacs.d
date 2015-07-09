(use-package css-mode
  :defer t
  :config
  (progn
    (setq css-indent-offset 2)
    (add-hook 'css-mode-hook
              (lambda ()
                (rainbow-mode 1)))))

(provide 'setup-css-mode)
