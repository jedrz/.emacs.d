;;; Set up auto pairing and wrapping region.

(eval-after-load "paredit"
  '(progn
     ;; Do not overwrite default key bindings
     (define-key paredit-mode-map (kbd "M-s") nil)
     (define-key paredit-mode-map (kbd "M-S") nil)
     (define-key paredit-mode-map (kbd "M-s M-s") 'paredit-splice-sexp)
     (define-key paredit-mode-map (kbd "M-s M-S") 'paredit-split-sexp)))

;; Automatically pair parentheses, quotes, etc.
(autopair-global-mode 1)

;; Disable autopair in elisp mode - use paredit instead.
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (autopair-mode -1)
            (paredit-mode 1)))

;; Handle triple quotes in python mode.
(add-hook 'python-mode-hook
          (lambda ()
            (setq autopair-handle-action-fns
                  (list #'autopair-default-handle-action
                        #'autopair-python-triple-quote-action))))

(defun enable-pairing-mode ()
  (wrap-region-mode -1)
  ;; Restore pairing mode if any was enabled.
  (when (boundp 'pairing-mode)
    (funcall pairing-mode 1)))

(defun enable-wrapping-mode ()
  (when (or paredit-mode autopair-mode)
    ;; Remember currently used pairing mode if any is enabled.
    (set (make-local-variable 'pairing-mode) (if paredit-mode
                                                 'paredit-mode
                                               'autopair-mode))
    (funcall pairing-mode -1))
  (wrap-region-mode 1))

(add-hook 'activate-mark-hook 'enable-wrapping-mode)
(add-hook 'deactivate-mark-hook 'enable-pairing-mode)

(provide 'setup-pairing)
