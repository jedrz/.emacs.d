;;; Basic configuration for css-mode

(defun css-mode-defaults ()
  (setq css-indent-offset 2)
  (rainbow-mode 1))

(add-hook 'css-mode-hook 'css-mode-defaults)

(provide 'setup-css-mode)
