;; Enable YaSnippet in every mode
(yas-global-mode 1)

;; Personal snippets
(setq yas-root-directory (concat user-emacs-directory "snippets"))

;; Load the snippets
(yas-load-directory yas-root-directory)

;; Select a snippet with popup library
(setq yas-prompt-functions '(yas-popup-prompt yas-no-prompt))

(provide 'setup-yasnippet)
