;; Shrink minor mode names

(after 'flyspell
  (diminish 'flyspell-mode))
(after 'whitespace
  (diminish 'global-whitespace-mode))
(after 'paredit
  (diminish 'paredit-mode "Par"))
(after 'undo-tree
  (diminish 'undo-tree-mode))
(after 'projectile
  (diminish 'projectile-mode "Prj"))
(after 'drag-stuff
  (diminish 'drag-stuff-mode))
(after 'elisp-slime-nav
  (diminish 'elisp-slime-nav-mode))
(after 'anzu
  (diminish 'anzu-mode))
(after 'highlight-symbol
  (diminish 'highlight-symbol-mode))

(provide 'setup-diminish)
