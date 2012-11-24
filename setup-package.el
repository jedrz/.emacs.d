;;; Configure package.el and install missing packages

(require 'package)

;; Packages archives
(defvar gnu '("gnu" . "http://elpa.gnu.org/packages/"))
(defvar marmalade '("marmalade" . "http://marmalade-repo.org/packages/"))
(defvar melpa '("melpa" . "http://melpa.milkbox.net/packages/"))

;; Add marmalade and melpa to package repos
(add-to-list 'package-archives marmalade t)
(add-to-list 'package-archives melpa t)

;; Initialize packages to be able to use them
(package-initialize)

(defvar my-packages
  '(
    ace-jump-mode
    auctex
    auto-complete                       ; depends on popup
    autopair
    browse-kill-ring
    buffer-move
    change-inner
    color-theme-sanityinc-solarized
    color-theme-sanityinc-tomorrow
    diminish
    dired+
    drag-stuff
    expand-region
    fill-column-indicator
    gist                                ; depends on gh, logito, pcache
    highlight-indentation
    ido-ubiquitous
    jump-char
    magit
    markdown-mode
    melpa
    multiple-cursors
    paredit
    pkgbuild-mode
    projectile
    rainbow-delimiters
    rainbow-mode
    refheap
    smart-forward
    smex
    solarized-theme
    undo-tree
    wgrep
    wrap-region
    yasnippet
    zenburn-theme
    zencoding-mode
    )
  "List of packages to be installed via package.el.")

;; Install missing packages
(let ((not-installed (remove-if 'package-installed-p my-packages)))
  (when not-installed
    (package-refresh-contents)
    (dolist (package not-installed)
      (package-install package))))

(provide 'setup-package)
