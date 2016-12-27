;; Fuzzy matching.
(use-package flx
  :ensure t
  :defer t)

;; isearch with an overview.
(use-package ivy
  :ensure t
  :defer t
  :bind
  (("C-c C-r" . ivy-resume))
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t
        ivy-re-builders-alist '((t . ivy--regex-fuzzy))
        ivy-initial-inputs-alist nil))

(use-package ivy-hydra
  :ensure t
  :defer t)

(use-package swiper
  :ensure t
  :defer t
  :bind
  (("C-s" . swiper)
   ("C-r" . swiper)
   ("C-S" . swiper-use-region)
   ("C-R" . swiper-use-region))
  :commands
  (swiper-use-region)
  :init
  (ivy-mode 1)
  :config
  (defun swiper-use-region ()
    "Search for active region or input."
    (interactive)
    (if (region-active-p)
        (let ((selection (buffer-substring-no-properties
                          (region-beginning) (region-end))))
          (deactivate-mark)
          (swiper selection))
      (call-interactively #'swiper))))

(use-package counsel
  :ensure t
  :defer t
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-x 4 C-f" . counsel-find-file-other-window)
   ("C-x C-i" . counsel-imenu)
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-h S" . counsel-info-lookup-symbol)
   ("C-x f" . counsel-recentf)
   ("C-x 4 f" . counsel-recentf-other-window))
  :commands
  (counsel-find-file-other-window counsel-recentf-other-window)
  :config
  (progn
    (setq counsel-find-file-at-point t)

    (ivy-set-actions
     #'counsel-find-file
     '(("j" find-file-other-window "other")
       ("s" ivy-sudo-edit-action "sudo")))

    (ivy-set-actions
     #'counsel-find-file-other-window
     '(("s" ivy-sudo-edit-action)))

    ;; Abbreviate /home/user with ~.
    (ivy-set-display-transformer #'counsel-recentf #'abbreviate-file-name)
    (ivy-set-display-transformer #'counsel-recentf-other-window #'abbreviate-file-name)

    (ivy-set-actions
     #'counsel-recentf
     '(("j" find-file-other-window "other")))

    (defun ivy-sudo-edit-action (file-name)
      (with-ivy-window
        (find-file (expand-file-name
                    (concat "/sudo:root@localhost:" file-name)
                    ivy--directory))))

    (defun counsel-find-file-other-window (&optional initial-input)
      "Forward to `find-file-other-window'.
When INITIAL-INPUT is non-nil, use it in the minibuffer during completion."
      (interactive)
      (ivy-read "Find file: " #'read-file-name-internal
                :matcher #'counsel--find-file-matcher
                :initial-input initial-input
                :action
                (lambda (x)
                  (with-ivy-window
                    (find-file-other-window (expand-file-name x ivy--directory))))
                :preselect (when counsel-find-file-at-point
                             (require 'ffap)
                             (let ((f (ffap-guesser)))
                               (when f (expand-file-name f))))
                :require-match #'confirm-after-completion
                :history #'file-name-history
                :keymap counsel-find-file-map
                :caller #'counsel-find-file-other-window))

    (defun counsel-recentf-other-window ()
      "Find a file on `recentf-list' in another window."
      (interactive)
      (ivy-read "Recentf: " recentf-list
                :action #'find-file-other-window
                :caller 'counsel-recentf-other-window))))

(provide 'setup-swiper)
