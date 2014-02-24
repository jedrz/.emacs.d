;;; Basic configuration for cc-mode and modes derived from it

(setq-default c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (other . "linux"))
              c-basic-offset 4)


(after 'auto-complete-clang
  (defun ac-clang-find-include-paths ()
    "Find includes paths using gcc and return as proper clang options."
    (let ((buffer (get-buffer-create "*gcc-include-paths*")))
      (with-current-buffer buffer
        (erase-buffer)
        (call-process "/bin/bash"
                      nil
                      buffer
                      nil
                      "-c"
                      "echo | g++ -v -x c++ -E -")
        (goto-char (point-min))
        (mapcar (lambda (path)
                  (concat "-I" path))
                (split-string
                 (buffer-substring-no-properties
                  (search-forward "#include <...> search starts here:" nil t)
                  (progn
                    (search-forward "End of search list.")
                    (line-beginning-position)))))))))

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Enable deleting all whitespace until next non-whitespace
            (c-toggle-hungry-state 1)
            ;; Do not indent open curly in in-class inline method
            (c-set-offset 'inline-open '0)
            ;; Set up completion with ac-clang
            (require 'auto-complete-clang)
            (add-to-list 'ac-sources 'ac-source-clang)
            (setq ac-clang-flags (ac-clang-find-include-paths))))

(provide 'setup-cc-mode)
