(add-hook 'ruby-mode-hook 'robe-mode)

(after 'company
  (add-to-list 'company-backends 'company-robe))

(provide 'setup-ruby-mode)
