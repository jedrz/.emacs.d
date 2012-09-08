;; Markdown
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\markdown$" . markdown-mode))
(add-hook 'markdown-mode-hook
          '(lambda ()
             (define-key markdown-mode-map (kbd "<tab>") 'yas/expand)))

(provide 'mode-mappings)
