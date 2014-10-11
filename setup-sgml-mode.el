;;; Basic configuration for sgml-mode and modes derived from it

;; Rename matching tags
(define-key sgml-mode-map (kbd "C-c C-r") 'mc/mark-sgml-tag-pair)

;; Tagedit
(tagedit-add-paredit-like-keybindings)
(tagedit-add-experimental-features)

;; FIXME: move zencoding configuration to a separate file?
(after 'zencoding-mode
  (define-key zencoding-mode-keymap (kbd "C-j") nil)
  (define-key zencoding-mode-keymap (kbd "C-<return>") nil)
  (define-key zencoding-mode-keymap (kbd "C-c C-z") 'zencoding-expand-line))

(add-hook 'sgml-mode-hook
          (lambda ()
            (zencoding-mode 1)
            (tagedit-mode 1)))

(provide 'setup-sgml-mode)
