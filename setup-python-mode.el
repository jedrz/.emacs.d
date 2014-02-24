;; Run inferior python as eldoc requires it
(add-hook 'python-mode-hook
          (lambda ()
            (run-python (python-shell-parse-command) nil nil)))
(add-hook 'python-mode-hook 'turn-on-eldoc-mode)

;; Setup jedi
(add-hook 'python-mode-hook 'jedi:setup)
(after 'jedi
  (setq jedi:complete-on-dot t)
  ;; Eldoc
  (setq jedi:tooltip-method nil))

(provide 'setup-python-mode)
