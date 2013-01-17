;; Load common lisp goodies
(require 'cl)

;; Set up load path
(add-to-list 'load-path user-emacs-directory)

;; Add extensions' folder and sub-folders to load path
(let ((default-directory (concat user-emacs-directory "vendor")))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

;; Configure package.el and install missing packages
(require 'setup-package)

;; Load functions in defuns-dir
(setq defuns-dir (concat user-emacs-directory "defuns"))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; Load appearance settings at the beginning to avoid momentary display
(require 'appearance)

;; Shrink minor mode names
(require 'setup-diminish)

;; Load general and extensions settings
(require 'generic)

;; Associate major modes with some files
(require 'mode-mappings)

;; Load settings for some major modes
(require 'setup-prog-mode)
(require 'setup-emacs-lisp-mode)
(eval-after-load "cc-mode" '(require 'setup-cc-mode))
(eval-after-load "python" '(require 'setup-python-mode))
(eval-after-load "sgml-mode" '(require 'setup-sgml-mode))
(eval-after-load "css-mode" '(require 'setup-css-mode))
(require 'setup-latex-mode)
(eval-after-load "asm-mode" '(require 'setup-asm-mode))

;; Setup extensions
(eval-after-load "dired" '(require 'setup-dired))
(eval-after-load "magit" '(require 'setup-magit))
(require 'setup-yasnippet)
(require 'setup-auto-complete)
(eval-after-load "hippie-exp" '(require 'setup-hippie-expand))
(require 'setup-pairing)
(require 'setup-flycheck)

;; Load extensions
(require 'smart-forward)
(require 'my-desktop)

;; Setup key bindings
(require 'key-bindings)
