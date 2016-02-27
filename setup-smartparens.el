(use-package smartparens
  :ensure t
  :defer t
  :init
  (smartparens-global-mode)
  :config
  (progn
    (require 'smartparens-config)

    ;; Highlights matching pairs
    (show-smartparens-global-mode)

    ;; Always skip closing pair even if the expression is not active
    (setq sp-autoskip-closing-pair 'always)

    (bind-key "C-M-f" #'sp-forward-sexp)
    (bind-key "C-M-b" #'sp-backward-sexp)

    (bind-key "C-M-n" #'sp-next-sexp)
    (bind-key "C-M-p" #'sp-previous-sexp)

    (bind-key "C-S-a" #'sp-beginning-of-sexp)
    (bind-key "C-S-e" #'sp-end-of-sexp)

    (bind-key "C-M-d" #'sp-down-sexp)
    (bind-key "C-M-S-d" #'sp-backward-down-sexp)

    (bind-key "C-M-u" #'sp-up-sexp)
    (bind-key "C-M-S-u" #'sp-backward-up-sexp)
    (bind-key "C-M-t" 'sp-transpose-sexp)

    (bind-key "C-M-k" #'sp-kill-sexp)
    (bind-key "C-M-w" #'sp-copy-sexp)

    (bind-key "C-)" #'sp-forward-slurp-sexp)
    (bind-key "C-}" #'sp-forward-barf-sexp)
    (bind-key "C-(" #'sp-backward-slurp-sexp)
    (bind-key "C-{" #'sp-backward-barf-sexp)

    (bind-key "M-s M-s" #'sp-splice-sexp)
    (bind-key "M-s M-S" #'sp-split-sexp)
    (bind-key "M-s M-c" #'sp-convolute-sexp)
    (bind-key "M-s M-a" #'sp-absorb-sexp)
    (bind-key "M-s M-e" #'sp-emit-sexp)
    (bind-key "M-s M-n" #'sp-add-to-next-sexp)
    (bind-key "M-s M-p" #'sp-add-to-previous-sexp)
    (bind-key "M-s M-j" #'sp-join-sexp)))

(provide 'setup-smartparens)
