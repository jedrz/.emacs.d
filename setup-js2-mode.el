(setq-default js2-basic-offset 2)

;; Let flycheck handle parse errors
(setq-default js2-show-parse-errors nil)
(setq-default js2-strict-missing-semi-warning nil)
(setq-default js2-strict-trailing-comma-warning t) ; jshint does not warn about this now for some reason

(js2r-add-keybindings-with-prefix "C-c r")

;; Tern
(add-hook 'js2-mode-hook 'tern-mode)
(after 'tern
  (tern-ac-setup))

(provide 'setup-js2-mode)
