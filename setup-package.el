;;; Configure package.el and install missing packages

(require 'package)

;; Add marmalade and melpa to package repos
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Initialize packages to be able to use them
(package-initialize)
;; If there are no archives get them
(unless package-archive-contents
  (package-refresh-contents))

(defvar my-packages
  '(
    ace-jump-mode
    anzu
    browse-kill-ring
    buffer-move
    change-inner
    color-theme-sanityinc-solarized
    color-theme-sanityinc-tomorrow
    diminish
    discover-my-major
    elisp-slime-nav
    evil-numbers
    expand-region
    fancy-narrow
    fill-column-indicator
    flx
    flx-ido
    gist                                ; depends on gh, logito, pcache
    git-messenger
    git-timemachine
    google-this
    highlight-indentation
    highlight-symbol
    ido-vertical-mode
    ido-ubiquitous
    jump-char
    latex-extra
    markdown-mode
    move-text
    multiple-cursors
    paradox
    pkgbuild-mode
    projectile
    rainbow-mode
    refheap
    scala-mode2
    smart-mode-line
    smex
    solarized-theme
    swiper
    undo-tree
    use-package
    visual-regexp
    wgrep
    yaml-mode
    zenburn-theme
    )
  "List of packages to be installed via package.el.")

;; Install missing packages
(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

(provide 'setup-package)
