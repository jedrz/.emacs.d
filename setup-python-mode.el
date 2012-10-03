;; Add python source to AC
(eval-after-load "auto-complete"
  '(require 'ac-python))

(add-hook 'python-mode-hook
          (lambda ()
            (turn-on-eldoc-mode)))

(provide 'setup-python-mode)
