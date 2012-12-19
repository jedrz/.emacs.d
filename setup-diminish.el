;; Shrink minor mode names

(eval-after-load "flyspell"
  '(diminish 'flyspell-mode))
(eval-after-load "whitespace"
  '(diminish 'global-whitespace-mode))
(eval-after-load "paredit"
  '(diminish 'paredit-mode "Par"))
(eval-after-load "undo-tree"
  '(diminish 'undo-tree-mode))
(eval-after-load "projectile"
  '(diminish 'projectile-mode "Prj"))
(eval-after-load "drag-stuff"
  '(diminish 'drag-stuff-mode))
(eval-after-load "elisp-slime-nav"
  '(diminish 'elisp-slime-nav-mode))

(provide 'setup-diminish)
