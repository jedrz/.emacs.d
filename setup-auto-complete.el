;;; Auto Complete configuration
;; source: https://github.com/purcell/emacs.d/blob/master/init-auto-complete.el

(require 'auto-complete)
(require 'auto-complete-config)

(global-auto-complete-mode 1)
(ac-config-default)

(setq ac-trigger-key "TAB"
      ac-auto-start nil ; start only by hitting TAB
      ac-dwim nil) ; To get pop-ups with docs even if a word is uniquely completed

(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)

;; Use Emacs' built-in TAB completion hooks to trigger AC
(setq tab-always-indent 'complete) ; use 'complete when auto-complete is disabled
(add-to-list 'completion-styles 'initials t)

;; Add auto-complete to some modes
(add-to-list 'ac-modes 'markdown-mode)

(provide 'setup-auto-complete)
