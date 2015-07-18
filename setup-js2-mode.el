(use-package js2-mode
  :ensure t
  :defer t
  :config
  (progn
    (setq-default js2-basic-offset 2)

    ;; Let flycheck handle parse errors.
    (setq-default js2-show-parse-errors nil)
    (setq-default js2-strict-missing-semi-warning nil)
    (setq-default js2-strict-trailing-comma-warning t)))

(use-package js2-refactor
  :ensure t
  :defer t
  :config
  (js2r-add-keybindings-with-prefix "C-c r"))

(use-package tern
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'js2-mode
    (add-hook 'js2-mode-hook 'tern-mode))
  :config
  (progn
    (setq tern-command (list (concat user-emacs-directory "vendor/tern/bin/tern")))))

(use-package company-tern
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-tern)))

(provide 'setup-js2-mode)
