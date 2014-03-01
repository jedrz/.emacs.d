;; Ido mode
(after 'ido
  (setq ido-enable-flex-matching t
        ido-everywhere t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-use-url-at-point t
        ;; Disable searching for other files while waiting for input.
        ido-auto-merge-work-directories-length -1
        ido-save-directory-list-file (concat user-emacs-directory "ido.last")))
(ido-mode 1)

;; Ido everywhere, really
(ido-ubiquitous-mode 1)

;; Better flex matching.
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;; Vertical ido
(ido-vertical-mode 1)

;; Smart M-x
(after 'smex
  (setq smex-save-file (concat user-emacs-directory "smex-items")))

;; Always rescan buffer for imenu
(after 'imenu
  (setq-default imenu-auto-rescan t))

(defmacro ido-ubiquitous-disable-compatibility (cmd package)
  "Disable emulating a quirk of `completing-read'.
If nothing is read, the first item from completion list is returned.
See `ido-ubiquitous-disable-compatibility' documentation for explanation."
  `(after ,package
     (defadvice ,cmd
       (around ido-ubiquitous-disable-compatibility activate compile)
       (let (ido-ubiquitous-enable-compatibility)
         ad-do-it))))

(ido-ubiquitous-disable-compatibility webjump 'webjump)

(provide 'setup-ido)
