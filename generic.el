;;; Generic settings

(eval-when-compile (require 'cl))

; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Recompile outdated compiled files at exit
(add-hook 'kill-emacs-hook 'byte-recompile-emacs-directory)

;; Backup files settings
(setq backup-by-copying t
      backup-directory-alist (list (cons "." (concat user-emacs-directory "backups")))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      ;; Make backup files even when they're in version control
      vc-make-backup-files t)

;; Autosave settings
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "autosaves/\\1") t)))

;; Create auto-save-list directory if doesn't exist yet
;; as its lack causes errors while auto saving is performed.
(let ((auto-save-list-dir (concat user-emacs-directory "auto-save-list")))
  (unless (file-exists-p auto-save-list-dir)
    (make-directory auto-save-list-dir)))

;; Savehist keeps track of some history
(after 'savehist
  (setq savehist-additional-variables
        ;; search entries
        '(search ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (concat user-emacs-directory "savehist")))
(savehist-mode 1)

;; Save recent files
(after 'recentf
  (setq recentf-save-file (concat user-emacs-directory "recentf")
        recentf-max-saved-items 200
        recentf-max-menu-items 15
        recentf-exclude '("ido\\.last" "\\.mc-lists\\.el" "/elpa/")
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

;; UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)

;; Spaces for indentation
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Set list of tab stop positions used by `tab-to-tab-stop'
(setq tab-stop-list (loop for tab from tab-width to 120 by tab-width
                          collect tab))

;; Set fill-column and comment-fill-column
(setq-default fill-column 79)
(after 'newcomment
  (setq comment-fill-column 70))

;; Sentences do not need double spaces to end.
(setq-default sentence-end-double-space nil)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Automatically open compressed files
(auto-compression-mode 1)

;; Enable set-goal-column
(put 'set-goal-column 'disabled nil)

;; Enable narrowing
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; Enable upcasing region
(put 'upcase-region 'disabled nil)

;; Enable downcasing region
(put 'downcase-region 'disabled nil)

(defadvice kill-line (after kill-line-cleanup-whitespace activate compile)
  "Cleanup white space after `kill-line' up to non white space character."
  (unless (bolp)
    (delete-region (point)
                   (progn (skip-chars-forward " \t") (point)))))

;; Set default dictionary for flyspell-mode
(after 'ispell
  (setq ispell-dictionary "english"))
(defvar ispell-my-dicts '("english" "polish")) ; used by ispell-cycle-dicts

;; Make edited files end with a carriage return
(setq require-final-newline t)

;; Remove trailing whitespace
;(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

;; Ido everywhere
(ido-ubiquitous-mode 1)

;; Smart M-x
(after 'smex
  (setq smex-save-file (concat user-emacs-directory "smex-items")))

;; Create imenu index
(setq-default imenu-auto-rescan t)

;; Any key deletes selection
(delete-selection-mode 1)

;; Revert buffers automatically associated with files when the file changes on disk
(global-auto-revert-mode 1)
;; Also auto refresh dired and be quiet
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;; Don't use M-TAB to correct words in flyspell-mode
(after 'flyspell
  (setq flyspell-use-meta-tab nil))

;; Handle camelCase words properly everywhere
(global-subword-mode 1)

;; Visualization of undo tree
(global-undo-tree-mode 1)

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; Projectile is a project interaction library
(projectile-global-mode 1)

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
  ;; Fix choosing first entry in webjump
  (defadvice webjump (around ido-ubiquitous-new activate compile)
    (let ((ido-ubiquitous-enable-compatibility nil))
      ad-do-it))
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

;; Text mode
(add-hook 'text-mode-hook
          (lambda ()
            (turn-on-auto-fill)
            (turn-on-flyspell)))

(provide 'generic)
