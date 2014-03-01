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
    ac-octave
    ace-jump-mode
    auto-complete                       ; depends on popup
    auto-complete-clang
    browse-kill-ring
    buffer-move
    change-inner
    color-theme-sanityinc-solarized
    color-theme-sanityinc-tomorrow
    diminish
    dired+
    dired-details
    elisp-slime-nav
    evil-numbers
    expand-region
    fill-column-indicator
    flx
    flx-ido
    flycheck
    flycheck-cask
    gist                                ; depends on gh, logito, pcache
    git-commit-mode
    highlight-indentation
    ido-ubiquitous
    jedi
    jump-char
    magit
    markdown-mode
    melpa
    move-text
    multiple-cursors
    pkgbuild-mode
    projectile
    rainbow-delimiters
    rainbow-mode
    refheap
    smartparens
    smex
    solarized-theme
    undo-tree
    wgrep
    yasnippet
    zenburn-theme
    zencoding-mode
    )
  "List of packages to be installed via package.el.")

;; Install missing packages
(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

(provide 'setup-package)
