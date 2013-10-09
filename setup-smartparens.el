;; smartparens configuration

(smartparens-global-mode 1)

(require 'smartparens-config)

;; Highlights matching pairs
(show-smartparens-global-mode t)

(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

(define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
(define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)

(define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
(define-key sp-keymap (kbd "C-S-e") 'sp-end-of-sexp)

(define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
(define-key sp-keymap (kbd "C-M-S-d") 'sp-backward-down-sexp)

(define-key sp-keymap (kbd "C-M-u") 'sp-up-sexp)
(define-key sp-keymap (kbd "C-M-S-u") 'sp-backward-up-sexp)

(define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
(define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)

(define-key sp-keymap (kbd "C-)") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-}") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "C-(") 'sp-backward-slurp-sexp)
(define-key sp-keymap (kbd "C-{") 'sp-backward-barf-sexp)

(define-key sp-keymap (kbd "M-s M-s") 'sp-splice-sexp)
(define-key sp-keymap (kbd "M-s M-S") 'sp-split-sexp)
(define-key sp-keymap (kbd "M-s M-c") 'sp-convolute-sexp)
(define-key sp-keymap (kbd "M-s M-a") 'sp-absorb-sexp)
(define-key sp-keymap (kbd "M-s M-e") 'sp-emit-sexp)
(define-key sp-keymap (kbd "M-s M-n") 'sp-add-to-next-sexp)
(define-key sp-keymap (kbd "M-s M-p") 'sp-add-to-previous-sexp)
(define-key sp-keymap (kbd "M-s M-j") 'sp-join-sexp)

;; TODO: add wrap-round M-(

(provide 'setup-smartparens)
