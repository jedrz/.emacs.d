;; Disable arrow keys
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))

(global-unset-key (kbd "C-<left>"))
(global-unset-key (kbd "C-<right>"))
(global-unset-key (kbd "C-<up>"))
(global-unset-key (kbd "C-<down>"))

(global-unset-key (kbd "M-<left>"))
(global-unset-key (kbd "M-<right>"))
(global-unset-key (kbd "M-<up>"))
(global-unset-key (kbd "M-<down>"))

;; Replace old M-x with smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Rebind C-a to work as M-m then second hit as usual C-a
(global-set-key (kbd "C-a") 'back-to-indentation-or-beginning)

;; Quickly go to word with ace-jump-mode
(global-set-key (kbd "C-c C-SPC") 'ace-jump-mode)
;; To char
(global-set-key (kbd "C-c C-c C-SPC") 'ace-jump-char-mode)
;; To line (there is still M-g g bind to goto-line)
(global-set-key (kbd "M-g M-g") 'ace-jump-line-mode)

;; Jump to a definition in the current file
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; Just join line
(global-set-key (kbd "C-x ^") 'join-line)

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
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

;;; Windows management
;; Use shift + arrow keys to switch between windows
(windmove-default-keybindings)
;; Windows rotating
(global-set-key (kbd "C-x w") 'rotate-windows)

;; A complementary binding to the apropos-command(C-h a)
(global-set-key (kbd "C-h A") 'apropos)

;; Google search
(global-set-key (kbd "C-x M-g") 'google-search)

(provide 'key-bindings)
