(eval-when-compile (require 'cl))

; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Enable disabled commands

;; Enable set-goal-column command
(put 'set-goal-column 'disabled nil)

;; Enable narrowing
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; Enable upcasing/downcasing a region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

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

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Sentences do not need double spaces to end.
(setq-default sentence-end-double-space nil)

;; Make edited files end with a carriage return
(setq require-final-newline t)

;; Do not break lines
(setq-default truncate-lines t)

;; Any key deletes selection
(delete-selection-mode 1)

;; Handle camelCase words properly everywhere
(global-subword-mode 1)

;; Remove trailing whitespace
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Automatically open compressed files
(auto-compression-mode 1)

;; Set default dictionary for flyspell-mode
(after 'ispell
  (setq ispell-dictionary "english"))
(defvar ispell-my-dicts '("english" "polish")) ; used by ispell-cycle-dicts

;; Revert buffers automatically associated with files when the file changes on disk
(global-auto-revert-mode 1)
;; Also auto refresh dired and be quiet
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;; Don't use M-TAB to correct words in flyspell-mode
(after 'flyspell
  (setq flyspell-use-meta-tab nil))

;; Custom hippie-expand expansion functions
(after 'hippie-exp
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

;; Visualization of undo tree
(global-undo-tree-mode 1)

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

(defadvice kill-line (after kill-line-cleanup-whitespace activate compile)
  "Cleanup white space after `kill-line' up to non white space character."
  (unless (bolp)
    (delete-region (point)
                   (progn (skip-chars-forward " \t") (point)))))

;; Text mode
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

(provide 'sane-defaults)
