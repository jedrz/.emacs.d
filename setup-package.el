;;; Configure package.el and install missing packages

;; When switching between emacs 23 and 24,
;; always use the bundled package.el in emacs 24
(let ((package-el-vendor-dir (expand-file-name (concat user-emacs-directory
                                                       "vendor/package"))))
  (when (and (file-directory-p package-el-vendor-dir)
             (>= emacs-major-version 24))
    (message "Removing local package.el to avoid shadowing bundled version")
    (setq load-path (remove package-el-vendor-dir load-path))))

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
    auto-complete
    autopair
    browse-kill-ring
    buffer-move
    change-inner
    color-theme-sanityinc-solarized
    color-theme-sanityinc-tomorrow
    dired+
    diminish
    drag-stuff
    expand-region
    fill-column-indicator
    ido-ubiquitous
    jump-char
    magit
    mark-multiple
    markdown-mode
    melpa
    multiple-cursors
    paredit
    pkgbuild-mode
    projectile
    rainbow-delimiters
    rainbow-mode
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

(defvar my-packages-emacs-23
  '(
    color-theme
    )
  "List of packages to be installed if emacs 23 is used.")

(defun install-missing-packages (packages)
  "Install missing PACKAGES."
  (let ((not-installed (remove-if 'package-installed-p my-packages)))
    (when not-installed
      (package-refresh-contents)
      (dolist (package not-installed)
        (package-install package)))))

;; Install missing packages
(install-missing-packages my-packages)

;; Install specific packages for emacs 23 if is already running
(when (< emacs-major-version 24)
  (install-missing-packages my-packages-emacs-23))

(provide 'setup-package)
