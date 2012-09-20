;; Hippie expand instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)

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

;; vim's f and b commands
(global-set-key (kbd "M-m") 'jump-char-forward)
(global-set-key (kbd "M-M") 'jump-char-backward)

;; Jump to a definition in the current file
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; Clever new lines
(global-set-key (kbd "C-<return>") 'new-line-below)
(global-set-key (kbd "C-S-<return>") 'new-line-above)
(global-set-key (kbd "M-<return>") 'new-line-in-between)

;; Sane open-line
(global-set-key (kbd "C-o") 'open-line-sane)
(global-set-key (kbd "C-S-o")
                (lambda () (interactive) (open-line-sane -1)))

;; Line movement
(global-set-key (kbd "C-S-<down>") 'move-line-down)
(global-set-key (kbd "C-S-<up>") 'move-line-up)

;; Use M-w for copy to end of line if no active region
(global-set-key (kbd "M-w") 'save-region-or-current-line)
;; M-W to copy entire line
(global-set-key (kbd "M-W")
                (lambda () (interactive) (save-region-or-current-line 1)))

;; Yank and indent
(global-set-key (kbd "C-S-y") 'yank-and-indent)

;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)

;; Multiple cursors!
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)

;; Rectangular region mode
(global-set-key (kbd "C-S-SPC") 'set-rectangular-region-anchor)

;; Nice replacement of string-rectangle
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)

;; Expand region
(global-set-key (kbd "C-=") 'er/expand-region)

;; Change inner/outer
(global-set-key (kbd "M-i") 'change-inner) ; which used to be tab-to-tab-stop
(global-set-key (kbd "M-o") 'change-outer) ; which used to be facemenu-set-bold

;; Smart forward
;; These key bindings replace making a selection using Shift
(global-set-key (kbd "C-S-f") 'smart-forward)
(global-set-key (kbd "C-S-b") 'smart-backward)
(global-set-key (kbd "C-S-n") 'smart-down)
(global-set-key (kbd "C-S-p") 'smart-up)

;;; File finding
;; Find recent files with ido
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
;; Find file in other window
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
;; Edit file with sudo
(global-set-key (kbd "M-s e") 'sudo-edit)
;; Replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Buffer file functions
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file) ; was find-file-read-only
(global-set-key (kbd "C-x C-S-k") 'delete-current-buffer-file)

;;; Windows management
;; Use shift + arrow keys to switch between windows
(windmove-default-keybindings)
;; Windows rotating
(global-set-key (kbd "C-x w") 'rotate-windows)

;; A complementary binding to the apropos-command(C-h a)
(global-set-key (kbd "C-h A") 'apropos)

;; Google search
(global-set-key (kbd "C-x M-g") 'google-search)

;; Show menu bar on demand
(global-set-key (kbd "<f12>") 'menu-bar-mode)

(provide 'key-bindings)
