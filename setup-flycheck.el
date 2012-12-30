;; Enable flycheck for all files
(add-hook 'find-file-hook 'flycheck-mode)

(eval-after-load "flycheck"
  '(progn
     ;; Define a poor c/c++ checker (it fails when errors affect other files,
     ;; not the one being being checked actually)
     (defmacro flycheck-define-clike-checker (name command modes)
       `(defvar ,(intern (format "flycheck-checker-%s" name))
          '(:command
            (,@command source-inplace)
            :error-patterns
            (("^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\): error: \\(.*\\)$"
              1 2 3 4 error)
             ("^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\): warning: \\(.*\\)$"
              1 2 3 4 warning))
            :modes ,modes)))
     (flycheck-define-clike-checker c
                                    ("gcc" "-fsyntax-only" "-Wall" "-Wextra")
                                    c-mode)
     (add-to-list 'flycheck-checkers 'flycheck-checker-c)
     (flycheck-define-clike-checker c++
                                    ("g++" "-fsyntax-only" "-Wall" "-Wextra")
                                    c++-mode)
     (add-to-list 'flycheck-checkers 'flycheck-checker-c++)))

(provide 'setup-flycheck)
