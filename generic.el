;;;; Generic settings

;; Packages archives
(defvar gnu '("gnu" . "http://elpa.gnu.org/packages/"))
(defvar marmalade '("marmalade" . "http://marmalade-repo.org/packages/"))
(defvar melpa '("melpa" . "http://melpa.milkbox.net/packages/"))

;; Add marmalade and melpa to package repos
(require 'package)
(add-to-list 'package-archives marmalade t)
(add-to-list 'package-archives melpa t)

(package-initialize)

; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Backup files settings
(setq backup-by-copying t
      backup-directory-alist (list (cons "." (concat user-emacs-directory "backups")))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Autosave settings
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Savehist keeps track of some history
(setq savehist-additional-variables
      ;; search entries
      '(search ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (concat user-emacs-directory "savehist"))
(savehist-mode 1)

;; Save recent files
(setq recentf-save-file (concat user-emacs-directory "recentf")
      recentf-max-saved-items 200
      recentf-max-menu-items 15
      recentf-auto-cleanup 'never)
(recentf-mode 1)

;; Save locations in files
(setq save-place-file (concat user-emacs-directory "saveplace"))
(setq-default save-place t)
(require 'saveplace)

;; Bookmarks
(setq bookmark-default-file (concat user-emacs-directory "bookmarks")
      bookmark-save-flag 1)

;; UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; Spaces for indentation
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Set fill-column and comment-fill-column
(setq-default fill-column 79)
(setq comment-fill-column 70)

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

;; Enable upcasing region
(put 'upcase-region 'disabled nil)

;; Enable downcasing region
(put 'downcase-region 'disabled nil)

;; Set default dictionary for flyspell-mode
(setq ispell-dictionary "english")

;; Enable system copy and paste
(setq x-select-enable-clipboard t)

;; Make edited files end with a carriage return
(setq require-final-newline t)

;; Remove trailing whitespace
;(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Ido mode
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-create-new-buffer 'always
      ido-save-directory-list-file (concat user-emacs-directory "ido.last"))
(ido-mode 1)

;; Ido everywhere
(ido-ubiquitous-mode 1)

;; Smart M-x
(setq smex-save-file (concat user-emacs-directory "smex-items"))
(smex-initialize)

;; Create imenu index
(setq-default imenu-auto-rescan t)

;; Any key deletes selection
(delete-selection-mode)

;; Revert buffers automatically associated with files when the file changes on disk
(global-auto-revert-mode 1)

;; Don't use M-TAB to correct words in flyspell-mode
(setq flyspell-use-meta-tab nil)

;; Handle camelCase words properly everywhere
(global-subword-mode 1)

;; Automatically pair parentheses and wrap region
(autopair-global-mode 1)
(add-hook 'emacs-lisp-mode-hook
          (lambda () (autopair-mode -1)))
(add-hook 'python-mode-hook
          #'(lambda ()
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))))

;; Drag lines, regions with M-<up/down/left/right>
(drag-stuff-global-mode 1)

;; Visualization of undo tree
(global-undo-tree-mode 1)

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; Projectile is a project interaction library
(projectile-global-mode 1)

;; Custom characters for ace-jump-mode
(eval-after-load 'ace-jump-mode
  '(setq ace-jump-mode-move-keys
         (nconc (loop for c from ?a to ?z collect c)
                (loop for c from ?A to ?Z collect c)
                (loop for c from ?0 to ?9 collect c)
                (loop for c in
                      '(?ą ?ć ?ę ?ł ?ó ?ś ?ż ?ź ?Ą ?Ć ?Ę ?Ł ?Ó ?Ś ?Ż ?Ź)
                      collect c))))

;; Text mode
(add-hook 'text-mode-hook
          (lambda ()
            (turn-on-auto-fill)
            (turn-on-flyspell)))
(provide 'generic)
