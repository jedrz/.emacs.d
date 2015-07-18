(use-package ido
  :defer t
  :init
  (ido-mode 1)
  :config
  (setq ido-enable-flex-matching t
        ido-everywhere t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-use-url-at-point t
        ;; Disable searching for other files while waiting for input.
        ido-auto-merge-work-directories-length -1
        ido-save-directory-list-file (concat user-emacs-directory "ido.last")))

;; Ido everywhere, really.
(use-package ido-ubiquitous
  :ensure t
  :defer t
  :init
  (ido-ubiquitous-mode 1)
  :config
  (progn
    (defmacro ido-ubiquitous-disable-compatibility (cmd package)
      "Disable emulating a quirk of `completing-read'.
If nothing is read, the first item from completion list is returned.
See `ido-ubiquitous-disable-compatibility' documentation for explanation."
      `(with-eval-after-load ,package
         (defadvice ,cmd
             (around ido-ubiquitous-disable-compatibility activate compile)
           (let (ido-ubiquitous-enable-compatibility)
             ad-do-it))))

    (with-eval-after-load 'webjump
      (ido-ubiquitous-disable-compatibility webjump 'webjump))))

;; Better flex matching.
(use-package flx-ido
  :ensure t
  :defer t
  :init
  (flx-ido-mode 1)
  :config
  ;; Disable ido faces to see flx highlights.
  (setq ido-use-faces nil))

;; Vertical ido
(use-package ido-vertical-mode
  :ensure t
  :defer t
  :init
  (ido-vertical-mode 1)
  :config
  ;; Don't override key bindings to browse history.
  (setq ido-vertical-define-keys nil)
  ;; Use C-n and C-p to navigate candidates.
  (add-hook 'ido-setup-hook
            (lambda ()
              (bind-key "C-n" #'ido-next-match ido-completion-map)
              (bind-key "C-p" #'ido-prev-match ido-completion-map))))

;; Smart M-x
(use-package smex
  :ensure t
  :defer t
  :config
  (setq smex-save-file (concat user-emacs-directory "smex-items")))

(use-package imenu
  :defer t
  :config
  ;; Always rescan buffer for imenu
  (setq-default imenu-auto-rescan t))

(provide 'setup-ido)
