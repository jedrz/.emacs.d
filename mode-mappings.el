;; Open .h files in c++ mode
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

;; JavaScript
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Markdown
(add-to-list 'auto-mode-alist '("\\.\\(md\\|markdown\\)$" . markdown-mode))

;; PKGBUILD
(add-to-list 'auto-mode-alist '("PKGBUILD" . pkgbuild-mode))

;; Treat .m files as octave files
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

(provide 'mode-mappings)
