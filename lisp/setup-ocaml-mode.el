(use-package tuareg
  :ensure t
  :defer t)

(use-package utop
  :ensure t
  :defer t
  :config
  ;; Use the opam installed utop
  (setq utop-command "opam exec -- utop -emacs")
  :hook (tuareg-mode . utop-minor-mode))

(use-package merlin
  :load-path (lambda () (list (expand-file-name "~/.opam/default/share/emacs/site-lisp")))
  :config
  (setq merlin-command (expand-file-name "~/.opam/default/bin/ocamlmerlin"))
  :hook (tuareg-mode . merlin-mode))

(provide 'setup-ocaml-mode)
