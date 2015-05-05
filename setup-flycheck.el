(require 's)

;; Enable flycheck for all buffers
(add-hook 'after-init-hook 'global-flycheck-mode)

(after 'flycheck
  ;; Highlight whole line with error
  (setq flycheck-highlighting-mode 'lines)

  ;; Define a poor c/c++ checker (it fails when errors affect other files,
  ;; not the one being being checked actually)
  (defmacro flycheck-define-clike-checker (name command modes)
    `(flycheck-define-checker ,(intern (format "%s" name))
       ,(format "A %s checker using %s" name (car command))
       :command (,@command source-inplace)
       :error-patterns
       ((warning line-start (file-name) ":" line ":" column
                 ": warning: " (message) line-end)
        (error line-start (file-name) ":" line ":" column
               ": error: " (message) line-end))
       :modes ',modes))
  (flycheck-define-clike-checker c-gcc
                                 ("gcc" "-fsyntax-only" "-Wall" "-Wextra")
                                 c-mode)
  (add-to-list 'flycheck-checkers 'c-gcc)
  (flycheck-define-clike-checker c++-g++
                                 ("g++" "-fsyntax-only" "-Wall" "-Wextra" "-std=c++11")
                                 c++-mode)
  (add-to-list 'flycheck-checkers 'c++-g++)

  ;; Display error messages on one line in minibuffer and by new lines
  ;; separated in `flycheck-error-message-buffer'.
  (defun flycheck-diplay-error-messages-one-line (errors)
    (-when-let (messages (-keep #'flycheck-error-message errors))
      (when (flycheck-may-use-echo-area-p)
        (message (s-join " | " messages))
        (with-current-buffer (get-buffer-create flycheck-error-message-buffer)
          (erase-buffer)
          (insert (s-join "\n\n" messages))))))
  (setq flycheck-display-errors-function
        'flycheck-diplay-error-messages-one-line)

  (add-hook 'flycheck-mode-hook 'flycheck-cask-setup))

(provide 'setup-flycheck)
