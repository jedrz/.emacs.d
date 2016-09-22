;;; Generate autoloads for some files.

(setq generated-autoload-file (concat user-emacs-directory "lisp/my-autoloads.el"))

(defun update-my-autoloads ()
  "Update autoloads for some files."
  (interactive)
  (mapc (lambda (path)
          (update-directory-autoloads (concat user-emacs-directory path)))
        '("defuns" "vendor" "vendor/atilde" "vendor/exttextcat")))

;; Create autoloads at startup if file doesn't exist.
(unless (file-exists-p generated-autoload-file)
  (update-my-autoloads))

;; Load the autoloads.
(load-file generated-autoload-file)

;; Update autoloads at exit.
(add-hook 'kill-emacs-hook 'update-my-autoloads)

(provide 'setup-autoloads)
