;; Run inferior python as eldoc requires it
(add-hook 'python-mode-hook
          (lambda ()
            (run-python (python-shell-parse-command) nil nil)))
(add-hook 'python-mode-hook 'turn-on-eldoc-mode)

;; Completion with anaconda
(add-hook 'python-mode-hook 'anaconda-mode)
(after 'company
  (add-to-list 'company-backends 'company-anaconda))

(provide 'setup-python-mode)
