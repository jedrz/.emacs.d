(eval-when-compile (require 'cl))

;; Recompile outdated compiled files at exit
(add-hook 'kill-emacs-hook 'byte-recompile-emacs-directory)

;; Savehist keeps track of some history
(after 'savehist
  (setq savehist-additional-variables '(search ring regexp-search-ring)
        savehist-file (concat user-emacs-directory "savehist")))
(savehist-mode 1)

;; Save recent files
(after 'recentf
  (setq recentf-save-file (concat user-emacs-directory "recentf")
        recentf-max-saved-items 200
        recentf-max-menu-items 15
        recentf-exclude '("ido\\.last" "\\.mc-lists\\.el" "/elpa/" "\\.git/")
        recentf-auto-cleanup 'never))
(recentf-mode 1)

;; Save locations in files
(after 'saveplace
  (setq save-place-file (concat user-emacs-directory "saveplace"))
  (setq-default save-place t))
(require 'saveplace)

;; Bookmarks
(after 'bookmark
  (setq bookmark-default-file (concat user-emacs-directory "bookmarks")
        bookmark-save-flag 1))

;; Projectile is a project interaction library
(projectile-global-mode 1)

;; Show the current function name in the header line only in prog modes.
(which-function-mode 1)
(add-hook 'prog-mode-hook
          (lambda ()
            (setq header-line-format
                  '((which-func-mode ("" which-func-format " "))))))
(setq mode-line-misc-info
      ;; We remove Which Function Mode from the mode line, because it's mostly
      ;; invisible here anyway.
      (assq-delete-all 'which-func-mode mode-line-misc-info))

;; Custom characters for ace-jump-mode
(after 'ace-jump-mode
  (setq ace-jump-mode-move-keys
        (nconc (loop for c from ?a to ?z collect c)
               (loop for c from ?A to ?Z collect c)
               (loop for c from ?0 to ?9 collect c)
               (loop for c in
                     '(?ą ?ć ?ę ?ł ?ó ?ś ?ż ?ź ?Ą ?Ć ?Ę ?Ł ?Ó ?Ś ?Ż ?Ź)
                     collect c))))

(after 'webjump
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

(after 'multiple-cursors-core
  (add-to-list 'mc/unsupported-minor-modes 'flyspell-mode))

;; Fancy narrow
(fancy-narrow-mode 1)
(setq fancy-narrow-lighter nil)

(provide 'my-misc)
