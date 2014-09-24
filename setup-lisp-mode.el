;;; Basic configuration for emacs lisp mode

(defconst my-elisp-mode-hooks
  '(emacs-lisp-mode-hook inferior-emacs-lisp-mode-hook))

(defconst my-lisp-mode-hooks
  (append my-elisp-mode-hooks
          '(lisp-mode-hook
            lisp-interaction-mode-hook
            clojure-mode-hook
            cider-repl-mode-hook)))

(defconst my-lisp-modes
  (-map (lambda (hook)
          (let ((hook-name (symbol-name hook)))
            (intern (substring hook-name 0
                               (- (length hook-name) (length "-hook"))))))
        my-lisp-mode-hooks))

(defun my-lisp-mode-setup ()
  (turn-on-eldoc-mode)
  (rainbow-delimiters-mode 1))

(defun my-elisp-mode-setup ()
  ;; Go to definition with M-. and back again with M-,
  (elisp-slime-nav-mode 1))

(--each my-lisp-mode-hooks
  (add-hook it 'my-lisp-mode-setup))

(--each my-elisp-mode-hooks
  (add-hook it 'my-elisp-mode-setup))

;; Slime
(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy slime-repl slime-company))
(after 'slime
  (require 'slime-company)
  (setq slime-auto-start 'always))

(after 'smartparens
  ;; paredit's wrap-round
  (sp-local-pair my-lisp-modes "(" nil :wrap "M-("
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

  ;; Enable strict mode in lisp modes
  (--each my-lisp-mode-hooks
    (add-hook it 'smartparens-strict-mode)))

(provide 'setup-lisp-mode)
