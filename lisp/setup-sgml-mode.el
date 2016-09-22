(use-package sgml-mode
  :defer t
  :config
  ;; Rename matching tags
  (with-eval-after-load 'multiple-cursors
    (bind-key "C-c C-r" #'mc/mark-sgml-tag-pair) sgml-mode-map))

(use-package tagedit
  :ensure t
  :defer t
  :init
  (add-hook 'sgml-mode-hook #'tagedit-mode)
  :config
  (progn
    (tagedit-add-paredit-like-keybindings)
    (tagedit-add-experimental-features)))

(use-package zencoding-mode
  :ensure t
  :defer t
  :init
  (add-hook 'sgml-mode-hook #'zencoding-mode)
  :config
  (progn
    (bind-key "C-j" nil zencoding-mode-keymap)
    (bind-key "C-<return>" nil zencoding-mode-keymap)
    (bind-key "C-c C-z" #'zencoding-expand-line) zencoding-mode-keymap))

(provide 'setup-sgml-mode)
