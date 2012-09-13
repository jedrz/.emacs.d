;; Set up load path
(add-to-list 'load-path user-emacs-directory)

;; Load functions in defuns-dir
(setq defuns-dir (concat user-emacs-directory "defuns"))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; Load stuff
(require 'generic)
(require 'appearance)
(require 'key-bindings)

;; Load settings for programming modes
(require 'setup-prog-mode)
(require 'setup-emacs-lisp-mode)
(require 'setup-c-mode)
(require 'setup-sgml-mode)
(require 'setup-css-mode)
(require 'setup-markdown-mode)

;; Load various settings
(require 'setup-yasnippet)
(require 'setup-auto-complete)
(require 'setup-hippie-expand)
