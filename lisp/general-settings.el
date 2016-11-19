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

;; Tab indents or completes.
(setq tab-always-indent 'complete)

;; Spaces for indentation.
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Set list of tab stop positions used by `tab-to-tab-stop'
(setq tab-stop-list (loop for tab from tab-width to 120 by tab-width
                          collect tab))

;; Set fill-column and comment-fill-column.
(setq-default fill-column 79)
(use-package newcomment
  :defer t
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

;; http://endlessparentheses.com/faster-pop-to-mark-command.html
;; When popping the mark, continue popping until the cursor actually moves.
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point))
        ad-do-it))))

(setq set-mark-command-repeat-pop t)

;; Set default dictionary for flyspell-mode.
(use-package ispell
  :defer t
  :config
  (setq ispell-dictionary "english"))

;; Detect buffer language.
(use-package exttextcat
  :defer t
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
  :defer t
  :diminish flyspell-mode
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
  :defer t
  :config
  ;; Start week at Monday.
  (setq calendar-week-start-day 1))

;; Visualization of undo tree.
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
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

;; `other-window' on steroids.
(use-package ace-window
  :ensure t
  :bind
  ("M-o" . ace-window)
  :config
  (setq aw-dispatch-always t))

(defadvice kill-line (after kill-line-cleanup-whitespace activate compile)
  "Cleanup white space after `kill-line' up to non white space character."
  (unless (bolp)
    (delete-region (point)
                   (progn (skip-chars-forward " \t") (point)))))

;; http://endlessparentheses.com/exclude-directories-from-grep.html
(use-package grep
  :defer t
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
  :diminish anzu-mode
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
  :config
  (progn
    (setq savehist-additional-variables '(search ring regexp-search-ring)
          savehist-file (concat user-emacs-directory "savehist"))
    (savehist-mode 1)))

;; Save recent files.
(use-package recentf
  :config
  (progn
    (setq recentf-save-file (concat user-emacs-directory "recentf")
          recentf-max-saved-items 2000
          recentf-max-menu-items 15
          recentf-exclude '("ido\\.last" "\\.mc-lists\\.el" "/elpa/" "\\.git/")
          recentf-auto-cleanup 3600)
    (recentf-mode 1)))

;; Save locations of files.
(use-package saveplace
  :config
  (progn
    (setq save-place-file (concat user-emacs-directory "saveplace"))
    (setq-default save-place t)))

;; Bookmarks.
(use-package bookmark
  :defer t
  :config
  (setq bookmark-default-file (concat user-emacs-directory "bookmarks")
        bookmark-save-flag 1))

(use-package webjump
  :defer t
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
  :diminish projectile-mode
  :init
  (progn
    (projectile-global-mode 1)
    (bind-key*
     "C-c p"
     (defhydra hydra-projectile (:color blue :hint nil :idle 0.4)
       "
    Files             Search          Buffer             Do
   ─────────────────────────────────────────────────────────────────────────────
    [_f_] file          [_a_] ag          [_b_] switch         [_g_] magit
    [_l_] file dwim     [_A_] grep        [_v_] show all       [_p_] commander
    [_r_] recent file   [_s_] occur       [_V_] ibuffer        [_i_] info
    [_d_] dir           [_S_] replace     [_K_] kill all
    [_o_] other         [_t_] find tag
    [_u_] test file     [_T_] make tags
    [_h_] root

    Other Window      Run             Cache
   ─────────────────────────────────────────────────────────────────────────────
    [_F_] file          [_U_] test        [_kc_] clear
    [_L_] dwim          [_m_] compile     [_kk_] add current
    [_D_] dir           [_c_] shell       [_ks_] cleanup
    [_O_] other         [_C_] command     [_kd_] remove
    [_B_] buffer
   -----------------------------------------------------------------------------
        "
       ("<tab>" hydra-master/body "back")
       ("a"   projectile-ag)
       ("A"   projectile-grep)
       ("b"   projectile-switch-to-buffer)
       ("B"   projectile-switch-to-buffer-other-window)
       ("c"   projectile-run-async-shell-command-in-root)
       ("C"   projectile-run-command-in-root)
       ("d"   projectile-find-dir)
       ("D"   projectile-find-dir-other-window)
       ("f"   projectile-find-file)
       ("F"   projectile-find-file-other-window)
       ("g"   projectile-vc)
       ("h"   projectile-dired)
       ("i"   projectile-project-info)
       ("kc"  projectile-invalidate-cache)
       ("kd"  projectile-remove-known-project)
       ("kk"  projectile-cache-current-file)
       ("K"   projectile-kill-buffers)
       ("ks"  projectile-cleanup-known-projects)
       ("l"   projectile-find-file-dwim)
       ("L"   projectile-find-file-dwim-other-window)
       ("m"   projectile-compile-project)
       ("o"   projectile-find-other-file)
       ("O"   projectile-find-other-file-other-window)
       ("p"   projectile-commander)
       ("r"   projectile-recentf)
       ("s"   projectile-multi-occur)
       ("S"   projectile-replace)
       ("t"   projectile-find-tag)
       ("T"   projectile-regenerate-tags)
       ("u"   projectile-find-test-file)
       ("U"   projectile-test-project)
       ("v"   projectile-display-buffer)
       ("V"   projectile-ibuffer))))
  :config
  (setq projectile-completion-system 'ivy))

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

(use-package avy
  :ensure t
  :defer t
  :bind
  ("M-m" . avy-goto-char-in-line)
  :init
  (bind-key
   "M-g"
   (defhydra hydra-avy (:color blue)
     "avy"
     ("c" avy-goto-char "char")
     ("C" avy-goto-char-2 "2 char")
     ("m" avy-goto-char-in-line "char in line")
     ("g" avy-goto-line "line")
     ("G" goto-line "line (default)")
     ("w" avy-goto-word-1 "word")
     ("W" avy-goto-word-0 "some word")
     ("s" avy-goto-subword-1 "subword")
     ("S" avy-goto-subword-0 "some subword"))))

(use-package imenu
  :defer t
  :config
  ;; Always rescan buffer for imenu
  (setq-default imenu-auto-rescan t))

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

;; Display available keybindings after delay.
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

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
  :diminish google-this-mode
  :bind
  ("C-x g" . google-this-mode-submap)
  :init
  (google-this-mode 1))

;; Paste buffers to refheap from emacs.
(use-package refheap
  :ensure t
  :defer t)

;; Weather from wttr.in.
(use-package wttrin
  :ensure t
  :defer t
  :commands (wttrin)
  :config
  (setq wttrin-default-cities '("Warsaw"
                                "Siedlce")))

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

;; Activate occur inside isearch with C-o.
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

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
(bind-key "M-t c" #'transpose-chars)
(bind-key "M-t l" #'transpose-lines)
(bind-key "M-t w" #'transpose-words)
(bind-key "M-t s" #'transpose-sexps)
(bind-key "M-t p" #'transpose-params)

;; Why there is no command to copy rectangle?
(bind-key "C-x r C" #'copy-rectangle)

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
(bind-key "C-x l"
          (defhydra hydra-launcher (:color blue)
            "launcher"
            ("l" lgrep "grep")
            ("r" rgrep "recursive grep")
            ("o" occur "occur")
            ("m" multi-occur "multi occur")
            ("p" paradox-list-packages "list packages")
            ("w" webjump "web jump")))

;; Toggle map.
(bind-key "C-x t"
          (defhydra hydra-toggle (:color blue)
            "toggle"
            ("f" auto-fill-mode "fill")
            ("t" truncate-lines "truncate")
            ("m" menu-bar-mode "menu bar")))

(bind-key "C-$" #'run-terminal-with-current-dir)

(bind-key "<f6>" #'exttextcat-guess-language-buffer)

(bind-key "<f2>"
          (defhydra hydra-zoom ()
            "zoom"
            ("+" text-scale-increase "in")
            ("-" text-scale-decrease "out")
            ("0" (text-scale-adjust 0) "reset" :color blue)))

(provide 'general-settings)
