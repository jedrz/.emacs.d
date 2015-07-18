(eval-when-compile
  (require 'cl))

;; Recompile outdated compiled files at exit.
(add-hook 'kill-emacs-hook #'byte-recompile-emacs-directory)

;; Savehist keeps track of some history.
(use-package savehist
  :init
  (savehist-mode 1)
  :config
  (setq savehist-additional-variables '(search ring regexp-search-ring)
        savehist-file (concat user-emacs-directory "savehist")))

;; Save recent files.
(use-package recentf
  :init
  (recentf-mode 1)
  :config
  (setq recentf-save-file (concat user-emacs-directory "recentf")
        recentf-max-saved-items 1000
        recentf-max-menu-items 15
        recentf-exclude '("ido\\.last" "\\.mc-lists\\.el" "/elpa/" "\\.git/")
        recentf-auto-cleanup 'never))

;; Save locations of files.
(use-package saveplace
  :config
  (progn
    (setq save-place-file (concat user-emacs-directory "saveplace"))
    (setq-default save-place t)))

;; Bookmarks.
(use-package bookmark
  :config
  (setq bookmark-default-file (concat user-emacs-directory "bookmarks")
        bookmark-save-flag 1))

;; Save current session before killing emacs.
(add-hook 'kill-emacs-hook #'my-desktop-kill-emacs-hook)

;; Projectile is a project interaction library.
(use-package projectile
  :ensure t
  :init
  (projectile-global-mode 1))

;; Show the current function name in the header line only in prog modes.
(which-function-mode 1)
(add-hook 'prog-mode-hook
          (lambda ()
            (setq header-line-format
                  '((which-func-mode ("" which-func-format " "))))))
(setq mode-line-misc-info
      ;; Remove Which Function Mode from the mode line, because it's mostly
      ;; invisible here anyway.
      (assq-delete-all 'which-func-mode mode-line-misc-info))

;; Custom characters for ace-jump-mode.
(use-package ace-jump-mode
  :ensure t
  :defer t
  :config
  (setq ace-jump-mode-move-keys
        (nconc (loop for c from ?a to ?z collect c)
               (loop for c from ?A to ?Z collect c)
               (loop for c from ?0 to ?9 collect c)
               (loop for c in
                     '(?ą ?ć ?ę ?ł ?ó ?ś ?ż ?ź ?Ą ?Ć ?Ę ?Ł ?Ó ?Ś ?Ż ?Ź)
                     collect c))))

(use-package webjump
  :config
  (setq webjump-sites (append
                       '(("bab.la" .
                          [simple-query
                           "bab.la"
                           "bab.la/slownik/angielski-polski/"
                           ""])
                         ("Urban Dictionary" .
                          [simple-query
                           "urbandictionary.com"
                           "urbandictionary.com/define.php?term="
                           ""]))
                       webjump-sample-sites)))

(use-package multiple-cursors
  :ensure t
  :defer t
  :config
  (add-to-list 'mc/unsupported-minor-modes 'flyspell-mode))

(use-package fancy-narrow
  :ensure t
  :defer 10
  :init
  (fancy-narrow-mode 1)
  :config
  (setq fancy-narrow-lighter nil))

(provide 'my-misc)
