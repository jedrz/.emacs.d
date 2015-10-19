(use-package cc-mode
  :mode ("\\.h$" . c++-mode)
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
    (add-hook 'c-mode-hook #'irony-mode)

    (defun my-irony-mode-hook()
      (define-key irony-mode-map [remap completion-at-point]
        'irony-completion-at-point-async)
      (define-key irony-mode-map [remap complete-symbol]
        'irony-completion-at-point-async))
    (add-hook 'irony-mode-hook #'my-irony-mode-hook)

    (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options))
  :config
  (setq irony-additional-clang-options '("-std=c++11")))

(use-package company-irony
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'irony
    (add-hook 'irony-mode-hook #'company-irony-setup-begin-commands))
  :config
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-irony)))

(provide 'setup-cc-mode)
