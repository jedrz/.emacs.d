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
    anaconda-mode
    anzu
    auctex
    auctex-latexmk
    browse-kill-ring
    buffer-move
    change-inner
    cider
    clj-refactor
    clojure-mode
    color-theme-sanityinc-solarized
    color-theme-sanityinc-tomorrow
    company
    company-anaconda
    company-auctex
    company-irony
    company-tern
    diff-hl
    diminish
    dired+
    dired-imenu
    discover-my-major
    elisp-slime-nav
    evil-numbers
    expand-region
    fancy-narrow
    fill-column-indicator
    flx
    flx-ido
    flycheck
    flycheck-cask
    gist                                ; depends on gh, logito, pcache
    git-commit-mode
    git-timemachine
    google-this
    highlight-indentation
    highlight-symbol
    ido-vertical-mode
    ido-ubiquitous
    irony
    js2-mode
    js2-refactor
    jump-char
    latex-extra
    magit
    markdown-mode
    move-text
    multiple-cursors
    org-journal
    paradox
    pkgbuild-mode
    projectile
    rainbow-delimiters
    rainbow-mode
    refheap
    robe
    slime
    slime-company
    smart-mode-line
    smartparens
    smex
    solarized-theme
    tagedit
    tern
    undo-tree
    visual-regexp
    wgrep
    yaml-mode
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
