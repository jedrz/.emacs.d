;; Open .h files in c++ mode
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

;; JavaScript
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Open Common Lisp files in lisp-mode
(add-to-list 'auto-mode-alist '("\\.cl$" . lisp-mode))

;; Zsh files in sh-mode
(add-to-list 'auto-mode-alist '("\\.zsh$" . sh-mode))

;; Markdown
(add-to-list 'auto-mode-alist '("\\.\\(md\\|markdown\\)$" . markdown-mode))

;; R
(autoload 'R-mode "ess-site")
(add-to-list 'auto-mode-alist '("\\.[rR]\\'" . R-mode))

;; PKGBUILD
(add-to-list 'auto-mode-alist '("PKGBUILD" . pkgbuild-mode))

;; Treat .m files as octave files
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;; org-journal - hacky solution
(autoload 'org-journal-mode "org-journal")
(add-to-list 'auto-mode-alist '("journal/" . org-journal-mode))

(provide 'mode-mappings)
