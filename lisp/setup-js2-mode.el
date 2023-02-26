(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx?\\'" . js2-jsx-mode))
  :config
  (setq-default js2-basic-offset 2)

  ;; Let flycheck handle parse errors.
  (setq-default js2-show-parse-errors nil)
  (setq-default js2-strict-missing-semi-warning nil)
  (setq-default js2-strict-trailing-comma-warning t))

(use-package js2-refactor
  :ensure t
  :defer t
  :config
  (js2r-add-keybindings-with-prefix "C-c r"))

;; Javascript development environment
(use-package indium
  :ensure t
  :defer t
  :init
  (add-hook 'js-mode-hook (lambda ()
                            (require 'indium)
                            (indium-interaction-mode)))
  :config
  (unbind-key "C-c d" indium-interaction-mode-map))

(use-package json-mode
  :ensure t
  :defer t)

(provide 'setup-js2-mode)
