(use-package octave
  :defer t
  :mode ("\\.m$" . octave-mode)
  :init
  (add-hook 'octave-mode-hook
            (lambda ()
              (run-octave t)
              (setq-local eldoc-documentation-function 'octave-eldoc-function)))
  :config
  (progn
    (setq octave-comment-char ?%)
    ;; Change prompt since in default one, version is not detected and no ouput
    ;; is printed in octave buffer.
    (setq inferior-octave-prompt "^[^>]+>+ ")))

(provide 'setup-octave-mode)
