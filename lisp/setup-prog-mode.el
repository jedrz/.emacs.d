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

(use-package company
  :ensure t
  :defer t
  :diminish company-mode
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  :config
  (progn
    (bind-key "C-n" #'company-select-next company-active-map)
    (bind-key "C-p" #'company-select-previous company-active-map)
    (bind-key "<tab>" #'company-complete company-active-map)
    (bind-key "TAB" #'company-complete company-active-map)

    ;; Workaround integration with yasnippet
    (with-eval-after-load 'yasnippet
      (defun company-indent-or-complete ()
        (interactive)
        (let* ((end-of-expression (looking-at "\\_>"))
               (org-easy-templating (and (eq major-mode #'org-mode)
                                         (save-excursion
                                           ;; Go to the beginning of line.
                                           (forward-line 0)
                                           (looking-at "<.\\_>"))))
               (complete-with-company (and end-of-expression (not org-easy-templating))))
          (if complete-with-company
              (company-complete-common)
            (let ((yas-fallback-behavior #'call-other-command))
              (yas--fallback)))))

      (setq yas-fallback-behavior '(apply company-indent-or-complete)))))

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

(use-package lsp-mode
  :ensure t
  :defer t
  :init
  ;; Overrides downcase-region.
  (setq lsp-keymap-prefix "C-x C-l")
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         ;; https://emacs-lsp.github.io/lsp-mode/page/lsp-typescript/
         (javascript-mode . lsp))
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :defer t
  :commands lsp-ui-mode)

(provide 'setup-prog-mode)
