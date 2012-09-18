;;; Basic configuration for sgml-mode and modes derived from it

(defun sgml-mode-defaults ()
  ;; nice key binding for renaming matching tags
  (local-set-key (kbd "C-c C-r") 'rename-sgml-tag)
  (zencoding-mode 1))

(add-hook 'sgml-mode-hook 'sgml-mode-defaults)

(provide 'setup-sgml-mode)
