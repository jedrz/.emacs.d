;;; Basic configuration for cc-mode and modes derived from it

(setq-default c-default-style "linux"
              c-basic-offset 4)

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Enable deleting all whitespace until next non-whitespace
            (c-toggle-hungry-state 1)))

(provide 'setup-cc-mode)
