(use-package dired
  :defer t
  :config
  (progn
    (require 'dired-x)

    (setq dired-listing-switches "-alhv --group-directories-first"
          ;; Copy to the second visiblef dired buffer.
          dired-dwim-target t
          ;; Revert buffer on revisiting.
          dired-auto-revert-buffer t
          dired-recursive-copies 'always
          dired-recursive-deletes 'always)

    (defun dired-goto-top ()
      "Move to the third line (..)."
      (interactive)
      (beginning-of-buffer)
      (search-forward ".." nil nil)
      (dired-move-to-filename))

    (bind-key [remap beginning-of-buffer] #'dired-goto-top dired-mode-map)

    (defun dired-goto-bottom ()
      "Move to the last file."
      (interactive)
      (end-of-buffer)
      (forward-line -1)
      (dired-move-to-filename))

    (bind-key [remap end-of-buffer] #'dired-goto-bottom dired-mode-map)

    (defadvice dired-clean-up-after-deletion
        (before dired-auto-kill-buffer-after-deletion activate compile)
      "Kill buffers associated with file or directory being removed.
Do not ask for permission."
      (let* ((fn (ad-get-arg 0))
             (buf (get-file-buffer fn))
             (buf-list (dired-buffers-for-dir (expand-file-name fn))))
        (and buf (kill-buffer buf))
        (mapc 'kill-buffer buf-list)))

    (with-eval-after-load 'wdired
      (bind-key [remap beginning-of-buffer] #'dired-goto-top wdired-mode-map)
      (bind-key [remap end-of-buffer] #'dired-goto-bottom) wdired-mode-map)))

(use-package dired-x
  :defer t
  :init
  (add-hook 'dired-mode-hook #'dired-omit-mode)
  :config
  ;; Omit hidden, autosave and lock files but show . and ..
  (setq dired-omit-files "^\\.?#\\|^\\.[^\\.].*$"))

(use-package dired+
  :ensure t
  :defer t
  :init
  (require 'dired+))

(use-package diff-hl
  :ensure t
  :defer t
  :init
  ;; Highlight changed files under vc.
  ;; TODO: Move to better file.
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode))

(use-package dired-imenu
  :ensure t
  :defer t
  :commands (dired-setup-imenu)
  :init
  (add-hook 'dired-mode-hook #'dired-setup-imenu))

(provide 'setup-dired)
