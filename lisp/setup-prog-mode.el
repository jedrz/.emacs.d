;;; Common settings for modes deriving from prog-mode

(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (turn-on-auto-fill))

(defun prog-mode-defaults ()
  "Default coding hook."
  (flyspell-prog-mode)
  (local-comment-auto-fill)
  (setq ispell-local-dictionary "english"))

(add-hook 'prog-mode-hook #'prog-mode-defaults)

(use-package eldoc
  :diminish eldoc-mode
  :init
  (global-eldoc-mode))

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

(use-package hl-todo
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook #'hl-todo-mode))

(use-package dumb-jump
  :ensure t
  :defer t
  :init
  (bind-key "C-M-g"
            (defhydra dumb-jump-hydra (:color blue :columns 3)
              "Dumb Jump"
              ("j" dumb-jump-go "Go")
              ("o" dumb-jump-go-other-window "Other window")
              ("e" dumb-jump-go-prefer-external "Go external")
              ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
              ("i" dumb-jump-go-prompt "Prompt")
              ("l" dumb-jump-quick-look "Quick look")
              ("b" dumb-jump-back "Back"))))

(provide 'setup-prog-mode)
