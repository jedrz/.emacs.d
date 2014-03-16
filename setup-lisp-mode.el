;;; Basic configuration for emacs lisp mode

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (turn-on-eldoc-mode)
            ;; Go to definition with M-. and back again with M-,
            (elisp-slime-nav-mode 1)
            (rainbow-delimiters-mode 1)))

(after 'smartparens
  ;; paredit's wrap-round
  (sp-local-pair 'emacs-lisp-mode "(" nil :wrap "M-("
                 :post-handlers '(:add my-restore-paren-location))

  ;; https://github.com/Fuco1/smartparens/wiki/Permissions#pre-and-post-action-hooks
  (defun my-add-space-after-sexp-insertion (id action _context)
    "Add space if pair of parens is followed by a sexp or word."
    (when (eq action 'insert)
      (save-excursion
        (forward-char (length (plist-get (sp-get-pair id) :close)))
        (when (or (eq (char-syntax (following-char)) ?w)
                  (looking-at (sp--get-opening-regexp)))
          (insert " ")))))

  ;; sp-wrap-with-pair doesn't execute post-handlers?
  (defun my-restore-paren-location ()
    "Move preceding paren to the previous line if it is empty."
    (let ((empty-line-above (save-excursion
                              (forward-line -1)
                              (looking-at "^\\s-*$"))))
      (when empty-line-above
        (save-excursion
          (forward-line -1)
          (delete-region (point) (1+ (line-end-position))))
        (save-excursion
          (newline-and-indent)))))

  ;; Enable strict mode in emacs lisp
  (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode))

(provide 'setup-emacs-lisp-mode)
