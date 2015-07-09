;; Auto completion for ruby
(use-package robe
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'ruby-mode-hook 'robe-mode)
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-robe))))

(provide 'setup-ruby-mode)
