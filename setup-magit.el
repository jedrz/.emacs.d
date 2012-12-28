;; From https://github.com/magnars/.emacs.d/blob/master/setup-magit.el

;; Restore the previous window configuration after quitting magit-status buffer

(defadvice magit-status (before magit-save-window-configuration activate)
  (window-configuration-to-register :magit-window-configuration))

(defadvice magit-quit-window (after magit-restore-window-configuration activate)
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
