;; HTML
(add-hook 'sgml-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-r") 'rename-sgml-tag)))

;; Markdown
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\markdown$" . markdown-mode))
(add-hook 'markdown-mode-hook
          '(lambda ()
             (define-key markdown-mode-map (kbd "<tab>") 'yas/expand)))

(provide 'mode-mappings)
