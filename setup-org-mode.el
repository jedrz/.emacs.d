;; Add some new modules.
(add-to-list 'org-modules 'org-habit)

;; Set up paths.
(setq org-directory "~/Dokumenty/org"
      org-default-notes-file (concat org-directory "/todo.org")
      ;; MobileOrg
      org-mobile-directory "~/Dropbox/MobileOrg"
      org-mobile-inbox-for-pull (concat org-directory "/from-mobile.org"))

;; Use ido completion.
(setq org-completion-use-ido t)

;; Do not split line when cursor in not at the end.
(setq org-M-RET-may-split-line nil)

;; Highlight source code.
(setq org-src-fontify-natively t)

;; TODO keywords.
;(setq org-todo-keywords
;      '((sequence "TODO" "NEXT" "SOMEDAY" "|" "DONE" "DELEGATED")))

;; Add a timestamp when a certain TODO item was finished.
(setq org-log-done 'time)

;; Align org tags before saving.
(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'org-align-all-tags nil t)))

;; Setup org-journal
(after 'org-journal
  (setq org-journal-dir (concat org-directory "/journal/")))

;; Org babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh . t)
   (python . t)))

;; LaTeX export
(after 'ox-latex
  (setq org-latex-pdf-process
        (-repeat 3 "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"))
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-listings 'minted)
  (add-to-list 'org-beamer-environments-extra
               '("onlyenv+block" "O" "\\begin{onlyenv}%a\\begin{block}{%h}" "\\end{block}\\end{onlyenv}")))

(after 'ox-beamer
  ;; Don't ask me if this variable can be evaluated.
  (put 'org-beamer-outline-frame-title 'safe-local-variable 'stringp))

(provide 'setup-org-mode)
