;; Add python source to AC
(eval-after-load 'auto-complete
    '(require 'ac-python))

(defun python-mode-defaults ()
  (turn-on-eldoc-mode))

(add-hook 'python-mode-hook 'python-mode-defaults)

(provide 'setup-python-mode)
