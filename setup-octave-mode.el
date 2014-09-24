(require 'ac-octave)

(setq octave-comment-char ?%)

(after 'octave-inf
  ;; Change prompt since in default one version is not detected and no ouput
  ;; is printed in octave buffer.
  (setq inferior-octave-prompt "^[^>]+>+ "))

;; Eldoc
;; http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/progmodes/octave.el
(when (not (fboundp 'octave-eldoc-function))
  (defun inferior-octave-process-live-p ()
    (process-live-p inferior-octave-process))

  (defcustom octave-eldoc-message-style 'auto
    "Octave eldoc message style: auto, oneline, multiline."
    :type '(choice (const :tag "Automatic" auto)
                   (const :tag "One Line" oneline)
                   (const :tag "Multi Line" multiline))
    :group 'octave
    :version "24.4")

  (defvar octave-eldoc-cache nil)

  (defun octave-eldoc-function-signatures (fn)
    (unless (equal fn (car octave-eldoc-cache))
      (inferior-octave-send-list-and-digest
       (list (format "print_usage ('%s');\n" fn)))
      (let (result)
        (dolist (line inferior-octave-output-list)
          (when (string-match
                 "\\s-*\\(?:--[^:]+\\|usage\\):\\s-*\\(.*\\)$"
                 line)
            (push (match-string 1 line) result)))
        (setq octave-eldoc-cache
              (cons (substring-no-properties fn)
                    (nreverse result)))))
    (cdr octave-eldoc-cache))

  (defun octave-eldoc-function ()
    "A function for `eldoc-documentation-function' (which see)."
    (when (inferior-octave-process-live-p)
      (let* ((ppss (syntax-ppss))
             (paren-pos (cadr ppss))
             (fn (save-excursion
                   (if (and paren-pos
                            ;; PAREN-POS must be after the prompt
                            (>= paren-pos
                                (if (eq (get-buffer-process (current-buffer))
                                        inferior-octave-process)
                                    (process-mark inferior-octave-process)
                                  (point-min)))
                            (or (not (eq (get-buffer-process (current-buffer))
                                         inferior-octave-process))
                                (< (process-mark inferior-octave-process)
                                   paren-pos))
                            (eq (char-after paren-pos) ?\())
                       (goto-char paren-pos)
                     (setq paren-pos nil))
                   (when (or (< (skip-syntax-backward "-") 0) paren-pos)
                     (thing-at-point 'symbol))))
             (sigs (and fn (octave-eldoc-function-signatures fn)))
             (oneline (mapconcat 'identity sigs
                                 (propertize " | " 'face 'warning)))
             (multiline (mapconcat (lambda (s) (concat "-- " s)) sigs "\n")))
        ;;
        ;; Return the value according to style.
        (pcase octave-eldoc-message-style
          (`auto (if (< (length oneline) (window-width (minibuffer-window)))
                     oneline
                   multiline))
          (`oneline oneline)
          (`multiline multiline))))))

(add-hook 'octave-mode-hook
          (lambda ()
            (run-octave t)
            (setq-local eldoc-documentation-function 'octave-eldoc-function)
            (eldoc-mode 1)))

(provide 'setup-octave-mode)
