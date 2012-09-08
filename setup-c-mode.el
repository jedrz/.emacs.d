;;; Basic configuration for cc-mode and modes derived from it

(defun c-mode-common-defaults ()
  (setq c-default-style "linux"
        c-basic-offset 4))

(add-hook 'c-mode-common-hook 'c-mode-common-defaults)

(provide 'setup-c-mode)
