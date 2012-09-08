;;; Basic configuration for emacs lisp mode

(defun emacs-lisp-mode-defaults ()
  ;; Arguments' names in minibuffer
  (turn-on-eldoc-mode)
  ;; rainbow parentheses
  (rainbow-delimiters-mode 1))

(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-mode-defaults)

(provide 'setup-emacs-lisp-mode)
