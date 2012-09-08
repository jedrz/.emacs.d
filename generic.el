;;;; Generic settings

;; Packages archives
(defvar gnu '("gnu" . "http://elpa.gnu.org/packages/"))
(defvar marmalade '("marmalade" . "http://marmalade-repo.org/packages/"))
(defvar melpa '("melpa" . "http://melpa.milkbox.net/packages/"))

;; Add marmalade and melpa to package repos
;(add-to-list 'package-archives marmalade t)
;(add-to-list 'package-archives melpa t)
(setq package-archives (list gnu marmalade melpa))

(package-initialize)

; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Backup files settings
(setq backup-by-copying t              ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.emacs.d/backups")) ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)               ; use versioned backups

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

;; bookmarks
(setq bookmark-default-file (concat user-emacs-directory "bookmarks")
      bookmark-save-flag 1)

;; Spaces for indentation
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Set kill-ring length
(setq kill-ring-max 500)

;; Set fill-column
(setq-default fill-column 79)

;; Set default dictionary for flyspell-mode
(setq ispell-dictionary "english")

;; Enable system copy and paste
(setq x-select-enable-clipboard t)

;; Make edited files end with a carriage return
(setq require-final-newline t)

;; Remove trailing whitespace
;(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Ido mode
(ido-mode 1)
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-create-new-buffer 'always
      ido-save-directory-list-file (concat user-emacs-directory "ido.last"))

;; Ido everywhere
(ido-ubiquitous-mode 1)

;; Smart M-x
(setq smex-save-file (concat user-emacs-directory "smex-items"))
(smex-initialize)

;; Create imenu index
(setq-default imenu-auto-rescan t)

;; Auto completion in minibuffer
(icomplete-mode 1)

;; Any key deletes selection
(delete-selection-mode)

;; Wrap region with punctuation, etc.
(wrap-region-global-mode 1)

;; Revert buffers automatically associated with files when the changes on disk
(global-auto-revert-mode 1)

;; Projectile is a project interaction library
(projectile-global-mode 1)

;;; Text mode
;; Turn on auto-fill mode in text-mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
;; Enable flyspell mode
;(add-hook 'text-mode-hook
;          '(lambda () (flyspell-mode 1)))

(provide 'generic)
