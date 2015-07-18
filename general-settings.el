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
  :bind
  (("M-/" . hippie-expand)
   ("C-M-/" . hippie-expand-lines))
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

(use-package ibuffer
  :bind
  ([remap list-buffers] . ibuffer))

(use-package calendar
  :config
  ;; Start week at Monday.
  (setq calendar-week-start-day 1))

;; Visualization of undo tree.
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1))

(use-package browse-kill-ring
  :ensure t
  :bind
  ("C-x C-y" . browse-kill-ring))

;; Undo/redo window configuration with C-c <left>/<right>.
(use-package winner
  :config
  (winner-mode 1))

;; Use shift + arrow keys to switch between buffers.
(use-package windmove
  :config
  (windmove-default-keybindings))

;; Buffers moving.
(use-package buffer-move
  :ensure t
  :bind
  (("<C-S-up>" . buf-move-up)
   ("<C-S-down>" . buf-move-down)
   ("<C-S-left>" . buf-move-left)
   ("<C-S-right>" . buf-move-right)))

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

;; Writable grep buffer.
(use-package wgrep
  :ensure t
  :defer t)

;; Show number of search matches in mode line.
(use-package anzu
  :ensure t
  :config
  (global-anzu-mode 1))

;; Visual query replace.
(use-package visual-regexp
  :ensure t
  :bind
  ("C-M-%" . vr/query-replace))

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
        recentf-auto-cleanup 3600))

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

(use-package ace-jump-mode
  :ensure t
  :bind
  (;; Quickly go to word with ace-jump-mode.
   ("C-c C-SPC" . ace-jump-mode)
   ;; To char, use C-u C-c C-SPC.
   ;; To line (there is still M-g g bound to goto-line).
   ("M-g M-g" . ace-jump-line-mode))
  :config
  ;; Custom characters for ace-jump-mode.
  (setq ace-jump-mode-move-keys
        (nconc (loop for c from ?a to ?z collect c)
               (loop for c from ?A to ?Z collect c)
               (loop for c from ?0 to ?9 collect c)
               (loop for c in
                     '(?ą ?ć ?ę ?ł ?ó ?ś ?ż ?ź ?Ą ?Ć ?Ę ?Ł ?Ó ?Ś ?Ż ?Ź)
                     collect c))))

(use-package fancy-narrow
  :ensure t
  :defer 10
  :init
  (fancy-narrow-mode 1)
  :config
  (setq fancy-narrow-lighter nil))

(use-package misc
  :bind
  (("M-z" . zap-up-to-char)
   ("M-Z" . zap-to-char)))

;; Move line or region.
(use-package move-text
  :ensure t
  :bind
  ("M-S-<down>" . move-text-down)
  ("M-S-<up>" . move-text-up))

(use-package multiple-cursors
  :ensure t
  :bind
  (;; From active region to multiple cursors.
   ("C-S-c C-S-c" . mc/edit-lines)
   ("C-S-c C-e" . mc/edit-ends-of-lines)
   ("C-S-c C-a" . mc/edit-beginnings-of-lines)
   ;; Often used.
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-S-c C->" . mc/mark-all-like-this-dwim)
   ;; Mark one occurrence.
   ("C-S-c w" . mc/mark-next-word-like-this)
   ("C-S-c W" . mc/mark-previous-word-like-this)
   ("C-S-c s" . mc/mark-next-symbol-like-this)
   ("C-S-c S" . mc/mark-previous-symbol-like-this)
   ("C-S-c e" . mc/mark-more-like-this-extended)
   ;; Mark many occurrences.
   ("C-S-c a a" . mc/mark-all-like-this)
   ("C-S-c a A" . mc/mark-all-like-this-in-defun)
   ("C-S-c a w" . mc/mark-all-words-like-this)
   ("C-S-c a W" . mc/mark-all-words-like-this-in-defun)
   ("C-S-c a s" . mc/mark-all-symbols-like-this)
   ("C-S-c a S" . mc/mark-all-symbols-like-this-in-defun)
   ("C-S-c a r" . mc/mark-all-in-region)
   ;; Rectangular region mode.
   ("C-S-SPC" . set-rectangular-region-anchor)
   ;; Add a cursor on click.
   ("C-S-<mouse-1>" . mc/add-cursor-on-click)
   ;; Add cursors for matches.
   ("C-S-c m" . vr/mc-mark)
   ;; Special.
   ("C-S-c n" . mc/insert-numbers)
   ("C-S-c s" . mc/sort-regions)
   ("C-S-c r" . mc/reverse-regions))
  :config
  (add-to-list 'mc/unsupported-minor-modes 'flyspell-mode))

(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

;; vim's f and b commands.
(use-package jump-char
  :ensure t
  :bind
  (("M-m" . jump-char-forward)
   ("M-M" . jump-char-backward)))

(use-package evil-numbers
  :ensure t
  :bind
  ;; Increase number at point.
  ("C-+" . evil-numbers/inc-at-pt))

;; Display major mode key bindings in popup menu.
(use-package discover-my-major
  :ensure t
  :bind
  ("C-h C-m" . discover-my-major))

(use-package google-this
  :ensure t
  :defer t
  :init
  (bind-key "g" #'google-this-mode-submap ctl-x-map))

;; Paste buffers to refheap from emacs.
(use-package refheap
  :ensure t
  :defer t)

;;; Core key bindings.

;; Kill also emacs daemon if started.
(bind-key "C-x r q" #'save-buffers-kill-emacs)
;; Usual C-x C-c close frame only.
(bind-key "C-x C-c" #'delete-frame)

;; Repeat last command.
(bind-key "C-z" #'repeat)             ; which used to be suspend-frame

;; C-x 4 with C-4.
(bind-key "C-4" #'ctl-x-4-prefix)

;; Paragraph movement.
(bind-key "M-n" #'forward-paragraph)
(bind-key "M-p" #'backward-paragraph)

;; Rebind C-a to work as M-m then second hit as usual C-a.
(bind-key "C-a" #'back-to-indentation-or-beginning)

;; Go to line with linum mode enabled.
(bind-key "M-g M-g" #'goto-line-with-feedback)

;; Like isearch but uses active region as search string.
(bind-key "C-S-s" #'isearch-forward-use-region)
(bind-key "C-S-r" #'isearch-backward-use-region)

;; Activate occur inside isearch with C-o.
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;; Jump to a definition in the current file.
(bind-key "C-x C-i" #'ido-imenu)

;; Clever new lines.
(bind-key "<C-return>" #'new-line-below)
(bind-key "<C-S-return>" #'new-line-above)
(bind-key "<M-return>" #'new-line-in-between)

;; Join line.
(bind-key "C-x j" #'join-line)
(bind-key "C-x J" (lambda (arg)
                    (interactive "p")
                    (join-line (- arg))))

;; Insert empty line below/above point.
(bind-key "C-o" #'empty-line-below)
(bind-key "C-S-o" #'empty-line-above)

;; M-S-SPC for deleting all spaces around point.
(bind-key "M-S-SPC" #'delete-horizontal-space)

;; Use M-w for copy to end of line if no active region.
(bind-key "M-w" #'save-region-or-current-line)
;; M-W to copy entire line.
(bind-key "M-W" (lambda () (interactive) (save-region-or-current-line 1)))

;; Yank and indent.
(bind-key "C-S-y" #'yank-and-indent)
(bind-key "M-Y" #'yank-pop-and-indent)

;; Duplicate region or current line
(bind-key "C-c d" #'duplicate-current-line-or-region)

;; Comment or uncomment region or current line
(bind-key "C-c c" #'comment-or-uncomment-current-line-or-region)

;; Indent region with a number of columns
(bind-key "C-x I" #'indent-rigidly)

;; Fill/unfill text
(bind-key "M-q" #'fill-paragraph-or-indent)
(bind-key "M-Q" #'unfill-paragraph)

;; Transpose stuff with M-t
(global-unset-key (kbd "M-t"))      ; which used to be transpose-words
(bind-key "M-t l" #'transpose-lines)
(bind-key "M-t w" #'transpose-words)
(bind-key "M-t s" #'transpose-sexps)
(bind-key "M-t p" #'transpose-params)

;; Why there is no command to copy rectangle?
(bind-key "C-x r C" #'copy-rectangle)

;;; File finding.
;; Find recent files with ido.
(bind-key "C-x f" #'recentf-ido-find-file)
;; Recent in other window.
;; Overwrites find-file-other-window (also bound to C-x 4 C-f).
(bind-key "C-x 4 f" (lambda () (interactive) (recentf-ido-find-file 1)))
;; Edit file with sudo.
(bind-key "C-x C-#" #'sudo-edit)

;; Buffer file functions.
(bind-key "C-x C-r" #'rename-current-buffer-file) ; was find-file-read-only
(bind-key "C-x C-S-k" #'delete-current-buffer-file)

;; Create a new scratch buffer.
(bind-key "C-x S" #'create-scratch-buffer)

;; Eval and replace anywhere.
(bind-key "C-x E" #'eval-and-replace)

;; A complementary binding to the apropos-command (C-h a).
(bind-key "C-h A" #'apropos)

;; Ispell word and save correction.
(bind-key "M-$" #'ispell-word-then-abbrev) ; was ispell-word

;; Launcher map.
;; http://endlessparentheses.com/launcher-keymap-for-standalone-features.html
(define-prefix-command 'launcher-map)
(define-key ctl-x-map "l" 'launcher-map)
(define-key launcher-map "l" 'lgrep)
(define-key launcher-map "r" 'rgrep)
(define-key launcher-map "o" 'occur)
(define-key launcher-map "m" 'multi-occur)
(define-key launcher-map "p" 'paradox-list-packages)
(define-key launcher-map "w" 'webjump)

;; Toggle map.
;; http://endlessparentheses.com/the-toggle-map-and-wizardry.html
(define-prefix-command 'toggle-map)
(define-key ctl-x-map "t" 'toggle-map)
(define-key toggle-map "f" 'auto-fill-mode)
(define-key toggle-map "m" 'menu-bar-mode)
(define-key toggle-map "t" 'my-themes-cycle)

(bind-key "C-$" #'run-terminal-with-current-dir)

(bind-key "<f6>" #'exttextcat-guess-language-buffer)

(provide 'general-settings)
