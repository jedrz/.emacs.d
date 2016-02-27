;; Fuzzy matching.
(use-package flx
  :ensure t
  :defer t)

;; isearch with an overview.
(use-package counsel
  :ensure t
  :defer t
  :bind
  (("C-s" . swiper)
   ("C-r" . swiper)
   ("C-S" . swiper-use-region)
   ("C-R" . swiper-use-region)
   ("C-c C-r" . ivy-resume)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-x 4 C-f" . counsel-find-file-other-window)
   ("C-x C-i" . counsel-imenu)
   ("C-x f" . ivy-recentf)
   ("C-x 4 f" . ivy-recentf-other-window)
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-h S" . counsel-info-lookup-symbol))
  :commands
  (swiper-use-region
   counsel-find-file-other-window
   ivy-recentf-other-window)
  :init
  (ivy-mode 1)
  :config
  (progn
    (setq ivy-use-virtual-buffers t
          ivy-re-builders-alist '((t . ivy--regex-fuzzy))
          ivy-initial-inputs-alist nil
          counsel-find-file-at-point t)

    ;; Abbreviate /home/user with ~.
    (ivy-set-display-transformer 'ivy-recentf 'abbreviate-file-name)

    (ivy-set-actions
     'counsel-find-file
     '(("j" find-file-other-window "other")
       ("s" ivy-sudo-edit-action "sudo")))

    (ivy-set-actions
     'counsel-find-file-other-window
     '(("s" ivy-sudo-edit-action)))

    (ivy-set-actions
     'ivy-recentf
     '(("j" find-file-other-window "other")))

    (defun swiper-use-region ()
      "Search for active region or input."
      (interactive)
      (if (region-active-p)
          (let ((selection (buffer-substring-no-properties
                            (region-beginning) (region-end))))
            (deactivate-mark)
            (swiper selection))
        (call-interactively 'swiper)))

    (defun counsel-find-file-other-window (&optional initial-input)
      "Forward to `find-file-other-window'.
When INITIAL-INPUT is non-nil, use it in the minibuffer during completion."
      (interactive)
      (ivy-read "Find file: " 'read-file-name-internal
                :matcher #'counsel--find-file-matcher
                :initial-input initial-input
                :action
                (lambda (x)
                  (find-file-other-window (expand-file-name x ivy--directory)))
                :preselect (when counsel-find-file-at-point
                             (require 'ffap)
                             (ffap-guesser))
                :require-match 'confirm-after-completion
                :history 'file-name-history
                :keymap counsel-find-file-map))

    (defun ivy-recentf-other-window ()
      "Find a file on `recentf-list' in another window."
      (interactive)
      (ivy-read "Recentf: " recentf-list
                :action #'find-file-other-window
                :caller 'ivy-recentf-other-window))

    (defun ivy-sudo-edit-action (file-name)
      (with-ivy-window
        (find-file (expand-file-name
                    (concat "/sudo:root@localhost:" file-name)
                    ivy--directory))))))

(provide 'setup-swiper)
