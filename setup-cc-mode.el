;;; Basic configuration for cc-mode and modes derived from it

(eval-after-load 'cc-mode
  '(setq c-default-style "linux"
         c-basic-offset 4))

(provide 'setup-cc-mode)
