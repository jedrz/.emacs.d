(use-package ess
  :ensure t
  :mode ("\\.[rR]\\'" . R-mode)
  :init
  (add-hook 'ess-mode-hook (lambda () (run-hooks #'prog-mode-hook)))
  :config
  (progn
    (setq ess-ask-for-ess-directory nil
          ess-eval-visibly nil
          ;; Keep global .Rhistory file.
          ess-history-directory "~/.R/")

    ;; Workaround to flush R history to file.
    ;; Apply for all buffers since emacs doesn't run `kill-buffer-hook'
    ;; which has `ess-kill-buffer-function'.
    ;; For persistent history in all buffers check:
    ;; https://oleksandrmanzyuk.wordpress.com/2011/10/23/a-persistent-command-history-in-emacs/
    (add-hook #'kill-emacs-hook (lambda ()
                                  (mapc (lambda (buffer)
                                          (with-current-buffer buffer
                                            (ess-kill-buffer-function)))
                                        (buffer-list))))

    (with-eval-after-load 'flycheck
      (setq flycheck-lintr-linters "with_defaults(camel_case_linter = NULL, snake_case_linter, object_usage_linter = NULL)"))))

(provide 'setup-ess-mode)
