(defmacro after (file &rest forms)
  "Evaluate FORMS after FILE is loaded."
  (declare (indent 1))
  `(eval-after-load ,file
     '(progn ,@forms)))

;; Set up load path
(add-to-list 'load-path user-emacs-directory)

;; Add extensions' folder and sub-folders to load path
(let ((default-directory (concat user-emacs-directory "vendor")))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

;; Generate some autoloads and load them
(require 'setup-autoloads)

;; Configure package.el and install missing packages
(require 'setup-package)

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
(after 'cc-mode (require 'setup-cc-mode))
(after 'python (require 'setup-python-mode))
(after 'sgml-mode (require 'setup-sgml-mode))
(after 'css-mode (require 'setup-css-mode))
(require 'setup-latex-mode)
(after 'asm-mode (require 'setup-asm-mode))

;; Setup extensions
(after 'dired (require 'setup-dired))
(after 'magit (require 'setup-magit))
(require 'setup-yasnippet)
(require 'setup-auto-complete)
(after 'hippie-exp (require 'setup-hippie-expand))
(require 'setup-pairing)
(require 'setup-flycheck)

;; Load extensions
(require 'smart-forward)
(require 'my-desktop)

;; Setup key bindings
(require 'key-bindings)
