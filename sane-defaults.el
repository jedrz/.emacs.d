(eval-when-compile
  (require 'cl))

; "y or n" instead of "yes or no".
(fset 'yes-or-no-p 'y-or-n-p)

;; Enable disabled commands.

;; Enable set-goal-column command.
(put 'set-goal-column 'disabled nil)

;; Enable narrowing.
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; Enable upcasing/downcasing a region.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Remove splash screen and message, change major mode.
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'emacs-lisp-mode)

;; Backup files settings.
(setq backup-by-copying t
      backup-directory-alist (list (cons "." (concat user-emacs-directory "backups")))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      ;; Make backup files even when they're in version control
      vc-make-backup-files t)

;; Autosave settings.
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "autosaves/\\1") t)))

;; Create auto-save-list directory if doesn't exist yet
;; as its lack causes errors while auto saving is performed.
(let ((auto-save-list-dir (concat user-emacs-directory "auto-save-list")))
  (unless (file-exists-p auto-save-list-dir)
    (make-directory auto-save-list-dir)))

;; UTF-8.
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)

;; .emacs.d in load path warning.
;; http://stackoverflow.com/a/25552511
(defadvice display-warning
    (around no-warn-emacs-directory-in-load-path (type msg &rest ignored)
            activate compile)
  (unless (and (eq type 'initialization)
               (string-prefix-p
                "Your `load-path' seems to contain\nyour `.emacs.d' directory"
                msg))
    ad-do-it))

;; Spaces for indentation.
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Set list of tab stop positions used by `tab-to-tab-stop'
(setq tab-stop-list (loop for tab from tab-width to 120 by tab-width
                          collect tab))

;; Set fill-column and comment-fill-column.
(setq-default fill-column 79)
(use-package newcomment
  :config
  (setq comment-fill-column 70))

;; Show keystrokes in progress.
(setq echo-keystrokes 0.1)

;; Sentences do not need double spaces to end.
(setq-default sentence-end-double-space nil)

;; Make edited files end with a carriage return.
(setq require-final-newline t)

;; Do not break lines.
(setq-default truncate-lines t)

;; Any key deletes selection.
(delete-selection-mode 1)

;; Handle camelCase words properly everywhere.
(global-subword-mode 1)

;; Move files to trash when deleting.
(setq delete-by-moving-to-trash t)

;; Automatically open compressed files.
(auto-compression-mode 1)

;; Set default dictionary for flyspell-mode.
(use-package ispell
  :config
  (setq ispell-dictionary "english"))

;; Detect buffer language.
(use-package exttextcat
  :config
  (add-hook 'find-file-hook #'exttextcat-guess-language-buffer))

(use-package autorevert
  :config
  (progn
    ;; Revert buffers automatically associated with files when the file changes
    ;; on disk.
    (global-auto-revert-mode 1)
    ;; Also auto refresh dired and be quiet.
    (setq global-auto-revert-non-file-buffers t
          auto-revert-verbose nil)))

;; Don't use M-TAB to correct words in flyspell-mode.
(use-package flyspell
  :config
  (setq flyspell-use-meta-tab nil))

(use-package hippie-exp
  :config
  ;; Custom hippie-expand expansion functions.
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                           try-expand-dabbrev-all-buffers
                                           try-expand-dabbrev-from-kill
                                           try-complete-file-name-partially
                                           try-complete-file-name
                                           try-expand-all-abbrevs
                                           try-expand-list
                                           try-expand-line
                                           try-complete-lisp-symbol-partially
                                           try-complete-lisp-symbol)))

;; Enable dynamic expansion of words.
(setq global-abbrev-table (make-abbrev-table)) ; Fix wrong type argument.
(setq-default abbrev-mode t)
(setq save-abbrevs 'silently)

(use-package calendar
  :config
  ;; Start week at Monday.
  (setq calendar-week-start-day 1))

;; Visualization of undo tree.
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1))

;; Undo/redo window configuration with C-c <left>/<right>.
(use-package winner
  :config
  (winner-mode 1))

(defadvice kill-line (after kill-line-cleanup-whitespace activate compile)
  "Cleanup white space after `kill-line' up to non white space character."
  (unless (bolp)
    (delete-region (point)
                   (progn (skip-chars-forward " \t") (point)))))

;; http://endlessparentheses.com/exclude-directories-from-grep.html
(use-package grep
  :config
  (progn
    (add-to-list 'grep-find-ignored-directories "auto")
    (add-to-list 'grep-find-ignored-directories "elpa")))

;; Show number of search matches in mode line.
(use-package anzu
  :ensure t
  :config
  (global-anzu-mode 1))

;; Text mode
(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'text-mode-hook #'flyspell-mode)

;; To avoid accidentally typing Alt Gr + Space that expands to strange
;; character I immediately replace it with just plain space.
(add-hook 'post-self-insert-hook
          (lambda ()
            (when (char-equal (char-before) ? )
              (delete-char -1)
              (insert-char ? ))))

(provide 'sane-defaults)
