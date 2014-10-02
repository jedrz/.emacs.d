;; Start credential-cache--daemon to remember passwords

(add-hook 'magit-push-hook 'magit-maybe-run-credential-cache-daemon)

(defvar magit--credential-cache-process nil)

(defun magit-maybe-run-credential-cache-daemon (&rest _)
  (let ((socket-path "~/.git-credential-cache/socket"))
    (unless (or (file-exists-p socket-path)
                magit--credential-cache-process)
      (setq magit--credential-cache-process
            (start-process "credential-cache-daemon" nil
                           "git" "credential-cache--daemon" socket-path)))))

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

(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

(provide 'setup-magit)
