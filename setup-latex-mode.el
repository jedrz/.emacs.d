;; Configure AUCTeX
(after 'tex
  (setq TeX-auto-save t         ; Automatically save style information
        ;; Parse document structure
        TeX-parse-self t
        ;; Use SyncTeX for source correlation
        TeX-source-correlate-method 'synctex
        ;; Enable source correlation mode
        TeX-source-correlate-mode t
        ;; Do not ask before deleting files
        TeX-clean-confirm nil
        ;; Set default pdf browser
        TeX-view-program-selection
        (cons '(output-pdf "Okular")
              (assq-delete-all 'output-pdf TeX-view-program-selection))
        ;; Don't ask to start server for inverse search
        TeX-source-correlate-start-server t)
  (setq-default TeX-master nil          ; Ask for master document
                ;; Generate output in PDF
                TeX-PDF-mode t))

(after 'tex-buf
  ;; Do not ask for permission before saving files
  (setq TeX-save-query nil))

;; Configure RefTex
(after 'reftex
  ;; Recommended optimizations
  (setq reftex-enable-partial-scans t
        reftex-save-parse-info t
        reftex-use-multiple-selection-buffers t
        ;; Plug RefTeX into AUCTeX
        reftex-plug-into-AUCTeX t))

(after 'latex
  (auctex-latexmk-setup))

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
