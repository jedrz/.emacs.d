(use-package python
  :defer t
  :config
  (progn
    ;; Run inferior python as eldoc requires it
    (add-hook 'python-mode-hook
              (lambda ()
                (run-python (python-shell-parse-command) nil nil)))
    (add-hook 'python-mode-hook #'turn-on-eldoc-mode)))

(use-package anaconda-mode
  :ensure t
  :defer t
  :init
  (add-hook 'python-mode-hook #'anaconda-mode))

(use-package company-anaconda
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-anaconda)))

(provide 'setup-python-mode)
