;; Load common lisp goodies
(require 'cl)

;; Set up load path
(add-to-list 'load-path user-emacs-directory)

;; Configure package.el and install missing packages
(require 'setup-package)

;; Add extensions' folder and sub-folders to load path
(let ((default-directory (concat user-emacs-directory "vendor")))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

;; Load functions in defuns-dir
(setq defuns-dir (concat user-emacs-directory "defuns"))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; Load stuff
(require 'generic)
(require 'appearance)

;; Associate major modes with some files
(require 'mode-mappings)

;; Load settings for programming modes
(require 'setup-prog-mode)
(require 'setup-emacs-lisp-mode)
(eval-after-load "cc-mode" '(require 'setup-cc-mode))
(eval-after-load "python" '(require 'setup-python-mode))
(eval-after-load "sgml-mode" '(require 'setup-sgml-mode))
(eval-after-load "css-mode" '(require 'setup-css-mode))
(eval-after-load "tex-site" '(require 'setup-latex-mode))

;; Load various settings
(require 'setup-yasnippet)
(require 'setup-auto-complete)
(eval-after-load "hippie-exp" '(require 'setup-hippie-expand))
(require 'setup-pairing)

;; Load extensions
(require 'change-inner)
(require 'smart-forward)
(require 'my-desktop)
(eval-after-load "dired" '(require 'dired+))

;; Setup key bindings
(require 'key-bindings)
