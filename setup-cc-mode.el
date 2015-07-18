(use-package cc-mode
  :defer t
  :config
  (progn
    (setq-default c-default-style '((java-mode . "java")
                                    (awk-mode . "awk")
                                    (other . "linux"))
                  c-basic-offset 4))

  (with-eval-after-load 'smartparens
    ;; https://github.com/Fuco1/smartparens/wiki/Permissions#pre-and-post-action-hooks
    (sp-local-pair '(c-mode c++-mode) "{" nil
                   :post-handlers '((my-create-newline-and-enter-sexp "RET"))))
  (defun my-create-newline-and-enter-sexp (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indent."
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  (add-hook 'c-mode-common-hook
            (lambda ()
              ;; Delete all whitespace until next non-whitespace.
              (c-toggle-hungry-state 1)
              ;; Do not indent open curly in in-class inline method.
              (c-set-offset 'inline-open 0))))

(use-package irony
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'c++-mode-hook #'irony-mode)
    (add-hook 'c-mode-hook #'irony-mode))
  :config
  (progn
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-irony))
    (add-hook 'irony-mode-hook #'company-irony-setup-begin-commands)))

(provide 'setup-cc-mode)
