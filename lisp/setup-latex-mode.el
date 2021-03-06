(use-package latex
  :ensure auctex
  :defer t
  :init
  (progn
    (add-hook 'LaTeX-mode-hook #'flyspell-mode-on)
    (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)))

(use-package tex-site
  :ensure auctex
  :defer t)

(use-package tex
  :defer t
  :config
  (progn
    (setq TeX-auto-save t       ; Automatically save style information.
          ;; Parse document structure.
          TeX-parse-self t
          ;; Use SyncTeX for source correlation.
          TeX-source-correlate-method 'synctex
          ;; Enable source correlation mode.
          TeX-source-correlate-mode t
          ;; Do not ask before deleting files.
          TeX-clean-confirm nil
          ;; Set default pdf browser.
          TeX-view-program-selection
          (cons '(output-pdf "Okular")
                (assq-delete-all 'output-pdf TeX-view-program-selection))
          ;; Don't ask to start server for inverse search.
          TeX-source-correlate-start-server t)
    (setq-default TeX-master nil        ; Ask for master document.
                  ;; Generate output in PDF.
                  TeX-PDF-mode t)
    (add-hook 'LaTeX-mode-hook #'add-watchwords)))

(use-package tex-buf
  :ensure auctex
  :defer t
  :config
  ;; Do not ask for permission before saving files.
  (setq TeX-save-query nil))

(use-package reftex
  :defer t
  :init
  (add-hook 'LaTeX-mode-hook #'reftex-mode)
  :config
  (setq reftex-enable-partial-scans t
        reftex-save-parse-info t
        reftex-use-multiple-selection-buffers t
        ;; Plug RefTeX into AUCTeX.
        reftex-plug-into-AUCTeX t))

(use-package latex-extra
  :ensure t
  :defer t
  :hook LaTeX-mode)

(use-package auctex-latexmk
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'latex
    (auctex-latexmk-setup)))

(use-package company-auctex
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'company
    (company-auctex-init)))

(use-package atilde
  :defer t
  :init
  :hook LaTeX-mode)

(provide 'setup-latex-mode)
