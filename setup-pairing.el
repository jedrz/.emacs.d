;;; Set up auto pairing and wrapping region.

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

(defvar pairing-mode 'paredit-mode
  "Currently used pairing mode (to restore it when mark becomes inactive).")

;; FIXME: this function doesn't work as expected.
(defun handle-pairing-modes ()
  "Enable or disable current pairing mode and wrap-region mode.

If region is active then current pairing mode is remembered and wrap-region
is enabled.
Otherwise wrap-region becomes disabled and remembered pairing mode
is restored."
  (if (region-active-p)
      (progn
        ;; Remember currently used pairing mode.
        (setq pairing-mode (if autopair-mode
                               'autopair-mode
                             'paredit-mode))
        (funcall pairing-mode -1)
        (wrap-region-mode 1))
    (wrap-region-mode -1)
    ;; Restore pairing mode.
    (funcall pairing-mode 1)))

;; (add-hook 'activate-mark-hook 'handle-pairing-modes)
;; (add-hook 'deactivate-mark-hook 'handle-pairing-modes)

(provide 'setup-pairing)
