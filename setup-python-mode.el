;; Add python source to AC
(after 'auto-complete
  (require 'ac-python))

;; Run inferior python as eldoc requires it
(add-hook 'python-mode-hook
          (lambda ()
            (run-python (python-shell-parse-command) nil nil)))
(add-hook 'python-mode-hook 'turn-on-eldoc-mode)

(provide 'setup-python-mode)
