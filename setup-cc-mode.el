;;; Basic configuration for cc-mode and modes derived from it

(eval-after-load "cc-mode"
  '(setq-default c-default-style "linux"
                 c-basic-offset 4))

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Enable deleting all whitespace until next non-whitespace with DEL
            ;; and auto inserting newlines after special characters
            (c-toggle-auto-hungry-state 1)))

(provide 'setup-cc-mode)
