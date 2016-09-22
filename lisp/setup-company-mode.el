(use-package company
  :ensure t
  :defer t
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  :config
  (progn
    (bind-key "C-n" #'company-select-next company-active-map)
    (bind-key "C-p" #'company-select-previous company-active-map)
    (bind-key "<tab>" #'company-complete company-active-map)
    (bind-key "TAB" #'company-complete company-active-map)

    ;; Workaround integration with yasnippet
    (with-eval-after-load 'yasnippet
      (defun company-indent-or-complete ()
        (interactive)
        (if (looking-at "\\_>")
            (company-complete-common)
          (let ((yas-fallback-behavior 'call-other-command))
            (yas--fallback))))

      (setq yas-fallback-behavior '(apply company-indent-or-complete)))

    ;; fci workaround - https://github.com/company-mode/company-mode/issues/180
    (defvar-local company-fci-mode-on-p nil)

    (defun company-turn-off-fci (&rest ignore)
      (when (boundp 'fci-mode)
        (setq company-fci-mode-on-p fci-mode)
        (when fci-mode (fci-mode -1))))

    (defun company-maybe-turn-on-fci (&rest ignore)
      (when company-fci-mode-on-p (fci-mode 1)))

    (add-hook 'company-completion-started-hook #'company-turn-off-fci)
    (add-hook 'company-completion-finished-hook #'company-maybe-turn-on-fci)
    (add-hook 'company-completion-cancelled-hook #'company-maybe-turn-on-fci)))

(provide 'setup-company-mode)
