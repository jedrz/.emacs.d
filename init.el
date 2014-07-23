;; Speed up Emacs?
(setq gc-cons-threshold 20000000)

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

;; My sane defaults for emacs
(require 'sane-defaults)

;; My misc
(require 'my-misc)

;; Associate major modes with some files
(require 'mode-mappings)

;; Load settings for some major modes
(require 'setup-prog-mode)
(after 'org (require 'setup-org-mode))
(require 'setup-lisp-mode)
(after 'clojure-mode (require 'setup-clojure-mode))
(after 'cc-mode (require 'setup-cc-mode))
(after 'python (require 'setup-python-mode))
(after 'sgml-mode (require 'setup-sgml-mode))
(after 'css-mode (require 'setup-css-mode))
(after 'js2-mode (require 'setup-js2-mode))
(after 'ruby-mode (require 'setup-ruby-mode))
(require 'setup-latex-mode)
(after 'asm-mode (require 'setup-asm-mode))
(after 'octave-mod (require 'setup-octave-mode))
(after 'prolog (require 'setup-prolog-mode))

;; Setup extensions
(require 'setup-ido)
(after 'dired (require 'setup-dired))
(after 'magit (require 'setup-magit))
(require 'setup-yasnippet)
(require 'setup-auto-complete)
(require 'setup-smartparens)
(require 'setup-flycheck)

;; Setup key bindings
(require 'key-bindings)
