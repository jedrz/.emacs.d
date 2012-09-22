;;; Latex configuration with Auctex

;; {{{
;; source: http://www.emacswiki.org/emacs/AUCTeX#toc20
;; (require 'dbus)

;; (defun un-urlify (fname-or-url)
;;   "A trivial function that replaces a prefix of file:/// with just /."
;;   (if (string= (substring fname-or-url 0 8) "file:///")
;;       (substring fname-or-url 7)
;;     fname-or-url))

;; (defun th-evince-sync (file linecol &rest ignored)
;;   (let* ((fname (un-urlify file))
;;          (buf (find-buffer-visiting fname))
;;          (line (car linecol))
;;          (col (cadr linecol)))
;;     (if (null buf)
;;         (message "[Synctex]: %s is not opened..." fname)
;;       (switch-to-buffer buf)
;;       (goto-line (car linecol))
;;       (unless (= col -1)
;;         (move-to-column col)))))

;; (defvar *dbus-evince-signal* nil)

;; (defun enable-evince-sync ()
;;   (require 'dbus)
;;   (when (and
;;          (eq window-system 'x)
;;          (fboundp 'dbus-register-signal))
;;     (unless *dbus-evince-signal*
;;       (setf *dbus-evince-signal*
;;             (dbus-register-signal
;;              :session nil "/org/gnome/evince/Window/0"
;;              "org.gnome.evince.Window" "SyncSource"
;;              'th-evince-sync)))))

;; (add-hook 'LaTeX-mode-hook 'enable-evince-sync)
;; }}}

;; Configure AUCTex
(eval-after-load 'auctex
  '(progn
     (setq TeX-auto-save t ; Automatically save style information
           ;; Parse document structure
           TeX-parse-self t
           ;; Use SyncTeX for source correlation
           TeX-source-correlate-method 'synctex
           ;; Enable source correlation mode
           TeX-source-correlate-mode t
           ;; Do not ask for permission before saving files
           TeX-save-query nil
           ;; Do not ask before deleting files
           TeX-clean-confirm nil
           ;; Please indent \item
           LaTeX-item-indent 0)
     (setq-default TeX-master nil ; Ask for master document
                   ;; Generate output in PDF
                   TeX-PDF-mode t)))

;; Configure RefTex
(eval-after-load 'reftex
  '(progn
     ;; Recommended optimizations
     (setq reftex-enable-partial-scans t
           reftex-save-parse-info t
           reftex-use-multiple-selection-buffers t
           ;; Plug RefTeX into AUCTeX
           reftex-plug-into-AUCTeX t)))

;; Configure flymake
(eval-after-load 'flymake
  '(defun flymake-get-tex-args (filename)
     "Get the command to check TeX documents on the fly."
     `("chktex" ("-v0" "-q" "-I",filename))))

;; Set up latex hooks
(defun latex-mode-my-hooks ()
  (flyspell-mode-on)
  (flymake-mode-on)
  (reftex-mode 1)
  (LaTeX-math-mode 1))

(add-hook 'LaTeX-mode-hook 'latex-mode-my-hooks)

(provide 'setup-latex-mode)
