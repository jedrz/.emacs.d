;;; Common settings for modes deriving from prog-mode

(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (turn-on-auto-fill))

(defun prog-mode-defaults ()
  "Default coding hook."
  (flyspell-prog-mode)
  (local-comment-auto-fill)
  (add-watchwords))

(add-hook 'prog-mode-hook #'prog-mode-defaults)

(use-package eldoc
  :diminish eldoc-mode)

;; Highlight changes made to files under vc.
(use-package diff-hl
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook #'diff-hl-mode))

;; Highlight symbol at point.
(use-package highlight-symbol
  :ensure t
  :defer t
  :diminish highlight-symbol-mode
  :init
  (add-hook 'prog-mode-hook #'highlight-symbol-mode)
  :config
  (progn
    (bind-key "M-s n" #'highlight-symbol-next prog-mode-map)
    (bind-key "M-s p" #'highlight-symbol-prev prog-mode-map)))

(use-package ag
  :ensure t
  :defer t)

(provide 'setup-prog-mode)
