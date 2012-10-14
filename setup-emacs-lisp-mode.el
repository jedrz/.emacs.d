;;; Basic configuration for emacs lisp mode

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (turn-on-eldoc-mode)
            (rainbow-delimiters-mode 1)))

(provide 'setup-emacs-lisp-mode)
