;; Kill also emacs daemon if started
(global-set-key (kbd "C-x r q") 'save-buffers-kill-emacs)
;; Usual C-x C-c close frame only
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; Hippie expand instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)
;; Expand line
(global-set-key (kbd "C-M-/") 'hippie-expand-lines)

;; Replace old M-x with smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; M-x without Meta
(global-set-key (kbd "C-x C-m") 'smex)

;; Rebind C-a to work as M-m then second hit as usual C-a
(global-set-key (kbd "C-a") 'back-to-indentation-or-beginning)

;; Quickly go to word with ace-jump-mode
(global-set-key (kbd "C-c C-SPC") 'ace-jump-mode)
;; To char, use C-u C-c C-SPC
;; To line (there is still M-g g bind to goto-line)
(global-set-key (kbd "M-g M-g") 'ace-jump-line-mode)

;; Scroll line down/up
(global-set-key (kbd "<down>") 'scroll-down-line)
(global-set-key (kbd "<up>") 'scroll-up-line)

;; Activate occur inside isearch with C-o
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;; Smart forward
;; These key bindings replace making a selection using Shift
(global-set-key (kbd "C-S-f") 'smart-forward)
(global-set-key (kbd "C-S-b") 'smart-backward)
(global-set-key (kbd "C-S-n") 'smart-down)
(global-set-key (kbd "C-S-p") 'smart-up)

;; vim's f and b commands
(global-set-key (kbd "M-m") 'jump-char-forward)
(global-set-key (kbd "M-M") 'jump-char-backward)

;; Jump to a definition in the current file
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; Clever new lines
(global-set-key (kbd "<C-return>") 'new-line-below)
(global-set-key (kbd "<C-S-return>") 'new-line-above)
(global-set-key (kbd "<M-return>") 'new-line-in-between)

;; Sane open-line
(global-set-key (kbd "C-o") 'open-line-sane)
(global-set-key (kbd "C-S-o") (lambda (arg)
                                (interactive "p")
                                (open-line-sane (- arg))))

;; Use M-w for copy to end of line if no active region
(global-set-key (kbd "M-w") 'save-region-or-current-line)
;; M-W to copy entire line
(global-set-key (kbd "M-W")
                (lambda () (interactive) (save-region-or-current-line 1)))

;; Yank and indent
(global-set-key (kbd "C-S-y") 'yank-and-indent)

;; Browse kill ring
(global-set-key (kbd "C-x C-y") 'browse-kill-ring)

;; Duplicate region or current line
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

;; Comment or uncomment region or current line
(global-set-key (kbd "C-c c") 'comment-or-uncomment-current-line-or-region)

;; Indent region with a number of columns
(global-set-key (kbd "C-x I") 'indent-rigidly)

;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)

;; Zap to char
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

;; Multiple cursors
;; From active region to multiple cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)
;; Mark mark like this
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c M-<") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "C-c M->") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "C-c M-a") 'mc/mark-all-in-region)
;; Rectangular region mode
(global-set-key (kbd "C-S-SPC") 'set-rectangular-region-anchor)

;; Mark multiple
;; Nice replacement of string-rectangle
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)

;; Why there is no command to copy rectangle?
(global-set-key (kbd "C-x r C") 'copy-rectangle)

;; Expand region
(global-set-key (kbd "C-=") 'er/expand-region)

;; Change inner/outer
(global-set-key (kbd "M-i") 'change-inner) ; which used to be tab-to-tab-stop
(global-set-key (kbd "M-o") 'change-outer) ; which used to be facemenu-set-bold

;;; File finding
;; Find recent files with ido
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
;; Recent in other window
;; Overwrites find-file-other-window (also bound to C-x 4 C-f).
(global-set-key (kbd "C-x 4 f")
                (lambda () (interactive) (recentf-ido-find-file 1)))
;; Edit file with sudo
(global-set-key (kbd "C-x C-#") 'sudo-edit)
;; Replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Buffer file functions
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file) ; was find-file-read-only
(global-set-key (kbd "C-x C-S-k") 'delete-current-buffer-file)

;; Jump from file to current directory
(autoload 'dired-jump "dired-x")
(autoload 'dired-jump-other-window "dired-x")
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x 4 C-j") 'dired-jump-other-window)

;; Occur with multiple buffers
(global-set-key (kbd "M-s m") 'multi-occur)

;; Recursive grep
(global-set-key (kbd "M-s r") 'rgrep)

;;; Windows management
;; Use shift + arrow keys to switch between windows
(windmove-default-keybindings)
;; Windows rotating
(global-set-key (kbd "C-x w") 'rotate-windows)
;; Windows moving
(global-set-key (kbd "<C-S-up>") 'buf-move-up)
(global-set-key (kbd "<C-S-down>") 'buf-move-down)
(global-set-key (kbd "<C-S-left>") 'buf-move-left)
(global-set-key (kbd "<C-S-right>") 'buf-move-right)

;; Create a new scratch buffer
(global-set-key (kbd "C-x S") 'create-scratch-buffer)

;; Eval and replace anywhere
(global-set-key (kbd "C-x E") 'eval-and-replace)

;; Magit status
(global-set-key (kbd "C-x m") 'magit-status) ; which used to be compose-mail

;; A complementary binding to the apropos-command(C-h a)
(global-set-key (kbd "C-h A") 'apropos)

;; Webjump
(global-set-key (kbd "C-x g") 'webjump)
;; Google search
(global-set-key (kbd "C-x M-g") 'google-search)

(global-set-key (kbd "C-$") 'run-urxvt-with-current-dir)

;; Show menu bar on demand
(global-set-key (kbd "<f12>") 'menu-bar-mode)

;; Switch between `my-themes'
(global-set-key (kbd "<f10>") 'my-themes-cycle)

(provide 'key-bindings)
