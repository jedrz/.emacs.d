;; Load autoloads since auctex is not installed from ELPA
;(load "auctex.el" nil t)
;(load "preview-latex.el" nil t)

;; Configure AUCTex
(after 'tex-site
  (setq TeX-auto-save t         ; Automatically save style information
        ;; Parse document structure
        TeX-parse-self t
        ;; Use SyncTeX for source correlation
        TeX-source-correlate-method 'synctex
        ;; Enable source correlation mode
        TeX-source-correlate-mode t
        ;; Do not ask for permission before saving files
        TeX-save-query nil
        ;; Do not ask before deleting files
        TeX-clean-confirm nil)
  (setq-default TeX-master nil          ; Ask for master document
                ;; Generate output in PDF
                TeX-PDF-mode t))

(after 'tex
  ;; Set default pdf browser
  (setq TeX-view-program-selection
        (cons '(output-pdf "Okular")
              (assq-delete-all 'output-pdf TeX-view-program-selection))))

;; Configure RefTex
(after 'reftex
  ;; Recommended optimizations
  (setq reftex-enable-partial-scans t
        reftex-save-parse-info t
        reftex-use-multiple-selection-buffers t
        ;; Plug RefTeX into AUCTeX
        reftex-plug-into-AUCTeX t))

;; Completion
(after 'company
  (company-auctex-init))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (flyspell-mode-on)
            (reftex-mode 1)
            (LaTeX-math-mode 1)
            (latex-extra-mode 1)
            (atilde-mode 1)))

(provide 'setup-latex-mode)
