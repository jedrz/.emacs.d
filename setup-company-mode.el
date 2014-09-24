(add-hook 'after-init-hook 'global-company-mode)

(after 'company
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "<tab>") 'company-complete)
  (define-key company-active-map (kbd "TAB") 'company-complete))

;; http://www.emacswiki.org/emacs/CompanyMode
(after 'yasnippet
  (defun company-indent-or-complete ()
    (interactive)
    (if (looking-at "\\_>")
        (company-complete-common)
      (indent-according-to-mode)))

  (setq yas-fallback-behavior '(apply company-indent-or-complete)))

(provide 'setup-company-mode)
