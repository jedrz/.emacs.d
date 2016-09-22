(use-package flycheck
  :ensure t
  :defer t
  :init
  ;; Enable flycheck for all buffers.
  (add-hook 'after-init-hook 'global-flycheck-mode)
  :config
  (progn
    ;; Highlight whole line with error.
    (setq flycheck-highlighting-mode 'lines)

    ;; Display error messages on one line in minibuffer and by new lines
    ;; separated in `flycheck-error-message-buffer'.

    (require 'subr-x)
    (require 'dash)

    (defun flycheck-diplay-error-messages-one-line (errors)
      (-when-let (messages (-keep #'flycheck-error-message errors))
        (when (flycheck-may-use-echo-area-p)
          (message (string-join messages " | "))
          (with-current-buffer (get-buffer-create flycheck-error-message-buffer)
            (erase-buffer)
            (insert (string-join messages "\n\n"))))))

    (setq flycheck-display-errors-function
          'flycheck-diplay-error-messages-one-line)

    ;; Integration with cask files.
    (add-hook 'flycheck-mode-hook 'flycheck-cask-setup)

    ;; Use c++11 standard.
    (setq-default flycheck-clang-language-standard "c++11"
                  flycheck-gcc-language-standard "c++11")))

(provide 'setup-flycheck)
