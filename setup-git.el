(use-package magit
  :ensure t
  :defer t
  :bind
  ("C-x m" . magit-status)             ; Which used to be compose-mail.
  :config
  (progn
    ;; Start credential-cache--daemon to remember passwords

    (add-hook 'magit-mode-hook #'magit-maybe-run-credential-cache-daemon)

    (defun magit-maybe-run-credential-cache-daemon (&rest _)
      (let ((socket-path (expand-file-name "~/.git-credential-cache/socket")))
        (unless (file-exists-p socket-path)
          (start-process "credential-cache-daemon" nil
                         "git" "credential-cache--daemon" socket-path))))

    ;; From https://github.com/magnars/.emacs.d/blob/master/setup-magit.el
    ;; Restore the previous window configuration after quitting magit-status buffer

    (defadvice magit-status
        (before magit-save-window-configuration activate compile)
      (window-configuration-to-register :magit-window-configuration))

    (defadvice magit-quit-window
        (after magit-restore-window-configuration activate compile)
      (jump-to-register :magit-window-configuration))

    ;; Ignore whitespace

    (defun magit-toggle-whitespace ()
      (interactive)
      (if (member "-w" magit-diff-options)
          (magit-dont-ignore-whitespace)
        (magit-ignore-whitespace)))

    (defun magit-ignore-whitespace ()
      (interactive)
      (add-to-list 'magit-diff-options "-w")
      (magit-refresh))

    (defun magit-dont-ignore-whitespace ()
      (interactive)
      (setq magit-diff-options (remove "-w" magit-diff-options))
      (magit-refresh))

    (bind-key "W" #'magit-toggle-whitespace magit-status-mode-map)))

(use-package git-messenger
  :ensure t
  :defer t)

(use-package git-timemachine
  :ensure t
  :defer t)

(provide 'setup-git)
