;;; Basic configuration for cc-mode and modes derived from it

(setq-default c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (other . "linux"))
              c-basic-offset 4)

(eval-after-load "semantic"
  '(require 'semantic/bovine/c))

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Enable deleting all whitespace until next non-whitespace
            (c-toggle-hungry-state 1)
            ;; Set up completion
            (semantic-mode 1)
            (add-to-list 'ac-sources 'ac-source-semantic)))

(provide 'setup-cc-mode)
