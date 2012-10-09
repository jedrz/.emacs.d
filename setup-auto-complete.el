;;; Auto Complete configuration
;; http://github.com/purcell/emacs.d/blob/master/init-auto-complete.el

(require 'auto-complete)
(require 'auto-complete-config)

(global-auto-complete-mode 1)
(ac-config-default)

(setq ac-auto-start nil ; To avoid segfaults in emacs 24.2
      ac-dwim nil ; To get pop-ups with docs even if a word is uniquely completed
      ac-use-menu-map t) ; To search for completions using C-s

;; Add workarounds
(eval-after-load "flyspell"
  '(ac-flyspell-workaround))
(eval-after-load "linum"
  '(ac-linum-workaround))

;; Default sources
(setq-default ac-sources '(ac-source-yasnippet
                           ac-source-dictionary
                           ac-source-words-in-buffer
                           ac-source-words-in-same-mode-buffers
                           ac-source-words-in-all-buffer))

(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)

;; Use Emacs' built-in TAB completion hooks to trigger AC
(setq tab-always-indent 'complete) ; use 'complete when auto-complete is disabled
(add-to-list 'completion-styles 'initials t)

;; hook AC into completion-at-point
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

;; Add auto-complete to some modes
(add-to-list 'ac-modes 'markdown-mode)

(provide 'setup-auto-complete)
