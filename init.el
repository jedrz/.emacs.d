;; Speed up Emacs?
(setq gc-cons-threshold 100000000)

;; Speed up Emacs?
(let ((file-name-handler-alist nil))

;; Don't load default library.
(setq inhibit-default-init t)

;; Don't load outdated byte-code files.
(setq load-prefer-newer t)

;; Start emacs server.
(require 'server)
(unless (server-running-p)
  (server-start))

;; Add custom emacs lisp files to load-path.
(add-to-list 'load-path (concat user-emacs-directory "lisp") t)

;; Add extensions folder and sub-folders to load path.
(let ((default-directory (concat user-emacs-directory "vendor")))
  (add-to-list 'load-path default-directory t)
  (normal-top-level-add-subdirs-to-load-path))

;; Generate some autoloads and load them.
(require 'setup-autoloads)

;; Configure package.el and install missing packages.
(require 'setup-package)

(require 'bind-key)
(require 'diminish)

(use-package dash
  :ensure t)

;; Separate custom file.
(setq custom-file (concat user-emacs-directory "lisp/custom.el"))
(load custom-file)

;; Load appearance settings at the beginning to avoid momentary display.
(require 'appearance)

;; Ensure hydra.
(use-package hydra
  :ensure t)

(require 'general-settings)

;; Load settings for some major modes.
(require 'setup-prog-mode)
(require 'setup-org-mode)
;; Need to load clojure first, because some clojure hooks are added in
;; setup-lisp-mode file.
(require 'setup-clojure-mode)
(require 'setup-lisp-mode)
(require 'setup-cc-mode)
(require 'setup-python-mode)
(require 'setup-sgml-mode)
(require 'setup-css-mode)
(require 'setup-js2-mode)
(require 'setup-ruby-mode)
(require 'setup-latex-mode)
(require 'setup-asm-mode)
(require 'setup-octave-mode)
(require 'setup-prolog-mode)
(require 'setup-ess-mode)
(require 'setup-rust-mode)
(require 'setup-sh-mode)
(require 'setup-haskell-mode)
(require 'setup-major-modes)

;; Setup extensions.
(require 'setup-dired)
(require 'setup-git)
(require 'setup-yasnippet)
(require 'setup-smartparens)
(require 'setup-flycheck)
(require 'setup-vertico)

)
