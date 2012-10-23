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

(defun handle-pairing-modes ()
  "Enable or disable current pairing mode and wrap-region mode.

If region is active then current pairing mode is remembered and wrap-region
is enabled.
Otherwise wrap-region becomes disabled and remembered pairing mode
is restored."
  (if (region-active-p)
      (progn
        ;; Remember currently used pairing mode if any is enabled.
        (when (or paredit-mode
                  autopair-mode)
          (set (make-local-variable 'pairing-mode) (if paredit-mode
                                                       'paredit-mode
                                                     'autopair-mode))
          (funcall pairing-mode -1))
        (wrap-region-mode 1))
    (wrap-region-mode -1)
    ;; Restore pairing mode if any was enabled.
    (when (boundp 'pairing-mode)
     (funcall pairing-mode 1))))

(add-hook 'activate-mark-hook 'handle-pairing-modes)
(add-hook 'deactivate-mark-hook 'handle-pairing-modes)

(provide 'setup-pairing)
