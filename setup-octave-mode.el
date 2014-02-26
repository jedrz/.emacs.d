(require 'ac-octave)

(after 'octave-inf
  ;; Change prompt since in default one version is not detected and no ouput
  ;; is printed in octave buffer.
  (setq inferior-octave-prompt "^[^>]+>+ "))

(defadvice ac-octave-init
  (after ac-octave-update-current-directoru activate compile)
  "Update current directory in octave buffer to the location of current file.

This allows local functions from current directory to be completed."
  (inferior-octave-send-list-and-digest
   (list (concat "cd " default-directory ";\n"))))

(add-hook 'octave-mode-hook
          (lambda ()
            (auto-complete-mode 1)
            (add-to-list 'ac-sources 'ac-source-octave)))

(provide 'setup-octave-mode)
