;;; Basic configuration for sgml-mode and modes derived from it

(eval-after-load 'sgml-mode
  ;; nice key binding for renaming matching tags
  '(define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag))

;; FIXME: move zencoding configuration to a separate file?
(eval-after-load 'zencoding-mode
  '(progn
     (define-key zencoding-mode-keymap (kbd "C-j") nil)
     (define-key zencoding-mode-keymap (kbd "C-<return>") nil)
     (define-key zencoding-mode-keymap (kbd "C-c C-z") 'zencoding-expand-line)))

(add-hook 'sgml-mode-hook
          (lambda ()
            (zencoding-mode 1)))

(provide 'setup-sgml-mode)
