;; Enable flycheck for all buffers
(global-flycheck-mode)

(after 'flycheck
  ;; Highlight whole line with error
  (setq flycheck-highlighting-mode 'lines)
  ;; Define a poor c/c++ checker (it fails when errors affect other files,
  ;; not the one being being checked actually)
  (defmacro flycheck-define-clike-checker (name command modes)
    `(flycheck-declare-checker ,(intern (format "flycheck-checker-%s" name))
       ,(format "A %s checker using %s" name (car command))
       :command '(,@command source-inplace)
       :error-patterns
       '(("^\\(?1:.*\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): error: \\(?4:.*\\)$"
          error)
         ("^\\(?1:.*\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): warning: \\(?4:.*\\)$"
          warning))
       :modes ',modes))
  (flycheck-define-clike-checker c
                                 ("gcc" "-fsyntax-only" "-Wall" "-Wextra")
                                 c-mode)
  (add-to-list 'flycheck-checkers 'flycheck-checker-c)
  (flycheck-define-clike-checker c++
                                 ("g++" "-fsyntax-only" "-Wall" "-Wextra")
                                 c++-mode)
  (add-to-list 'flycheck-checkers 'flycheck-checker-c++))

(provide 'setup-flycheck)
