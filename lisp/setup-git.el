(use-package magit
  :ensure t
  :defer t
  :bind
  ("C-x m" . magit-status)             ; Which used to be compose-mail.
  :config
  (progn
    ;; Silence, magit.
    (setq magit-push-always-verify nil)

    ;; Integrate with gopass.
    (add-to-list 'magit-process-find-password-functions
                 'magit-process-password-auth-source)

    ;; Start credential-cache--daemon to remember passwords.

    (add-hook 'magit-mode-hook #'magit-maybe-run-credential-cache-daemon)

    (defun magit-maybe-run-credential-cache-daemon (&rest _)
      (let ((socket-path (expand-file-name "~/.git-credential-cache/socket")))
        (unless (file-exists-p socket-path)
          (start-process "credential-cache-daemon" nil
                         "git" "credential-cache--daemon" socket-path))))

    ;; From https://github.com/magnars/.emacs.d/blob/master/setup-magit.el
    ;; Restore the previous window configuration after quitting magit-status
    ;; buffer.

    (defadvice magit-status
        (before magit-save-window-configuration activate compile)
      (window-configuration-to-register :magit-window-configuration))

    (defadvice magit-quit-window
        (after magit-restore-window-configuration activate compile)
      (jump-to-register :magit-window-configuration))

    (defun magit-visit-origin ()
      (interactive)
      (let ((remote-url (magit-get "remote" "origin" "url")))
        (browse-url remote-url)))

    (bind-key "H" #'magit-visit-origin magit-status-mode-map)))

(use-package forge
  :ensure t
  :defer t
  :config
  (add-to-list 'forge-alist
               '("gitlab.touk.pl"
                 "gitlab.touk.pl/api/v4"
                 "gitlab.touk.pl"
                 forge-gitlab-repository)))

;; Provides function that popup commit message at current line. This is useful
;; when you want to know why this line was changed.
(use-package git-messenger
  :ensure t
  :defer t)

;; Step through historic versions of git controlled file using.
(use-package git-timemachine
  :ensure t
  :defer t)

(use-package git-auto-commit-mode
  :ensure t
  :defer t)

(provide 'setup-git)
