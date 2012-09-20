;;; Basic configuration for emacs lisp mode

(defun emacs-lisp-mode-defaults ()
  (turn-on-eldoc-mode)
  (paredit-mode 1)
  (rainbow-delimiters-mode 1))

(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-mode-defaults)

(provide 'setup-emacs-lisp-mode)
