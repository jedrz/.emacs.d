(require 'ac-octave)

(after 'octave-inf
  ;; Change prompt since in default one version is not detected and no ouput
  ;; is printed in octave buffer.
  (setq inferior-octave-prompt "^[^>]+>+ "))

(defun inferior-octave-update-current-directory ()
  "Update current directory in octave buffer to the location of current file.

This allows to local functions from current directory be completed."
  (when inferior-octave-process
    (inferior-octave-send-list-and-digest
     (list (concat "cd " default-directory ";\n")))))

(add-hook 'octave-mode-hook
          (lambda ()
            (auto-complete-mode 1)
            (add-to-list 'ac-sources 'ac-source-octave)
            (inferior-octave-update-current-directory)))

(provide 'setup-octave-mode)
