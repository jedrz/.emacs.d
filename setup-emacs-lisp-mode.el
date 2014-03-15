;;; Basic configuration for emacs lisp mode

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (turn-on-eldoc-mode)
            ;; Go to definition with M-. and back again with M-,
            (elisp-slime-nav-mode 1)
            (rainbow-delimiters-mode 1)))

(after 'smartparens
  ;; paredit's wrap-round
  (sp-local-pair 'emacs-lisp-mode "(" nil :wrap "M-(")
  ;; Enable strict mode in emacs lisp
  (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode))

(provide 'setup-emacs-lisp-mode)
