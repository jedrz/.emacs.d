;;; Configure package.el and use-package.

(require 'package)

;; Add marmalade and melpa to package repos.
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; Bootstrap use-package.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Package.el on steroids.
(use-package paradox
  :ensure t
  :defer t
  :config
  (setq paradox-execute-asynchronously nil))

(provide 'setup-package)
