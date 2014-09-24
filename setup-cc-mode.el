;;; Basic configuration for cc-mode and modes derived from it

(setq-default c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (other . "linux"))
              c-basic-offset 4)


;; (defun ac-clang-find-include-paths ()
;;   "Find include paths using gcc and return as proper clang options."
;;   (with-temp-buffer
;;     (call-process "/bin/bash"
;;                   nil
;;                   (current-buffer)
;;                   nil
;;                   "-c"
;;                   "echo | g++ -v -x c++ -E -")
;;     (goto-char (point-min))
;;     (mapcar (lambda (path)
;;               (concat "-I" path))
;;             (split-string
;;              (buffer-substring-no-properties
;;               (search-forward "#include <...> search starts here:" nil t)
;;               (progn
;;                 (search-forward "End of search list.")
;;                 (line-beginning-position)))))))

(after 'smartparens
  ;; https://github.com/Fuco1/smartparens/wiki/Permissions#pre-and-post-action-hooks
  (sp-local-pair '(c-mode c++-mode) "{" nil
                 :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  (defun my-create-newline-and-enter-sexp (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indent."
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode)))

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Enable deleting all whitespace until next non-whitespace
            (c-toggle-hungry-state 1)
            ;; Do not indent open curly in in-class inline method
            (c-set-offset 'inline-open '0)))

;; Completion with irony
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(after 'company
  (add-to-list 'company-backends 'company-irony))
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

(provide 'setup-cc-mode)
