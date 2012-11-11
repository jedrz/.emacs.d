;; Open .h files in c++ mode
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

;; Markdown
(add-to-list 'auto-mode-alist '("\\.\\(md\\|markdown\\)$" . markdown-mode))

;; PKGBUILD
(add-to-list 'auto-mode-alist '("PKGBUILD" . pkgbuild-mode))

(provide 'mode-mappings)
