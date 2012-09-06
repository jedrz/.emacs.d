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

;; Make enter to work as C-j
(global-set-key (kbd "RET") 'newline-and-indent)

;; Replace old M-x with smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;;; Motion
;; Rebind C-a to back-to-indentation
(global-set-key (kbd "C-a") 'back-to-indentation)
;; Jump to a definition in the current file
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;;; Editing
;; Just join line
(global-set-key (kbd "C-x ^") 'join-line)
;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)

;;; File finding
;; Find recent files with ido
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
;; Find file in other window
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
;; Edit file with sudo
(global-set-key (kbd "M-s e") 'sudo-edit)
;; Replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; Windows management
;; Use shift + arrow keys to switch between windows
(windmove-default-keybindings)
;; Windows rotating
(global-set-key (kbd "C-x w") 'rotate-windows)

;; A complementary binding to the apropos-command(C-h a)
(global-set-key (kbd "C-h A") 'apropos)

(provide 'key-bindings)
