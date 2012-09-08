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
(require 'mode-mappings)
(require 'setup-prog-mode)
(require 'setup-yasnippet)
