(use-package python
  :defer t
  :config
  (progn
    ;; Run inferior python as eldoc requires it
    ;; Well, maybe not.
    ;; (add-hook 'python-mode-hook
    ;;           (lambda ()
    ;;             (run-python (python-shell-parse-command) nil nil)))
    (add-hook 'python-mode-hook #'turn-on-eldoc-mode)

    (add-hook 'inferior-python-mode-hook #'turn-on-comint-history)))

(use-package anaconda-mode
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'python-mode-hook #'anaconda-mode)
    (add-hook 'python-mode-hook #'anaconda-eldoc-mode)))

(use-package company-anaconda
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-anaconda)))

(use-package virtualenvwrapper
  :ensure t
  :defer t)

(provide 'setup-python-mode)
