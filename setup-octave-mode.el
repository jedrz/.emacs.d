(require 'ac-octave)

(after 'octave-inf
  ;; Change prompt since in default one version is not detected and no ouput
  ;; is printed in octave buffer.
  (setq inferior-octave-prompt "^[^>]+>+ "))

(add-hook 'octave-mode-hook
          (lambda ()
            (auto-complete-mode 1)
            (add-to-list 'ac-sources 'ac-source-octave)))

(provide 'setup-octave-mode)
