;; Configuration based on http://doc.norang.ca/org-mode.html.

(use-package org
  :ensure t
  :pin org
  :defer t
  :bind
  (("C-c a" . org-agenda)
   ("C-c l" . org-store-link)
   ("C-c b" . org-iswitchb)
   ("C-c k" . org-capture))
  :config
  (progn
    ;; Indent (view only) headlines and text.
    (setq org-startup-indented t)

    ;; Single key navigation for headlines.
    (setq org-use-speed-commands t)

    ;; Add some new modules.
    (add-to-list 'org-modules 'org-habit)

    ;; Set up paths.
    (setq org-directory "~/Dokumenty/org"
          ;; File for capturing new tasks.
          org-default-notes-file (concat org-directory "/notes.org")
          org-agenda-files (list (concat org-directory "/todo.org")
                                 org-default-notes-file))

    (setq org-todo-keywords
          '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
            (sequence "WAITING(w@)" "HOLD(h@)" "|" "CANCELLED(c)")))

    (setq org-todo-keyword-faces
          '(("NEXT" :foreground "blue" :weight bold)
            ("WAITING" :foreground "orange" :weight bold)
            ("HOLD" :foreground "magenta" :weight bold)
            ("CANCELLED" :foreground "forest green" :weight bold)))

    (setq org-capture-templates
          (let ((refile-file (concat org-directory "/notes.org")))
            `(("t" "todo" entry (file ,refile-file)
               "* TODO %?")
              ("n" "note" entry (file ,refile-file)
               "* %?"))))

    ;; Refile setup.
    (setq org-refile-targets '((org-agenda-files :level . 1))
          org-refile-use-outline-path 'file)

    ;; Do not split line when cursor in not at the end.
    (setq org-M-RET-may-split-line nil)

    ;; Highlight source code.
    (setq org-src-fontify-natively t)

    ;; Add a timestamp when a certain TODO item was finished.
    (setq org-log-done 'time)

    ;; Custom timestamp formats.
    (setq-default org-display-custom-times t)
    (setq org-time-stamp-custom-formats
          '("<%d-%m-%Y %a>" . "<%d-%m-%Y %a %H:%M>"))

    (setq org-ellipsis "⤵")

    ;; Hide characters like *word*, etc.
    (setq org-hide-emphasis-markers t)

    ;; Index more levels with imenu.
    (setq org-imenu-depth 5)

    ;; Show entities as UTF8 characters.
    (setq org-pretty-entities t)

    ;; Align org tags before saving.
    (add-hook 'org-mode-hook
              (lambda ()
                (add-hook 'before-save-hook 'org-align-all-tags nil t)))

    ;; Org babel
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (sh . t)
       (python . t)
       ;; (R . t)
       (calc . t)
       (sql . t)))

    ;; Never evaluate blocks when exporting.
    (setq org-export-babel-evaluate nil)

    ;; Better bullets.
    (font-lock-add-keywords #'org-mode
                            '(("^ +\\([-*]\\) "
                               (0 (prog1 () (compose-region
                                             (match-beginning 1)
                                             (match-end 1)
                                             "•"))))))))

(use-package org-plus-contrib
  :ensure t
  :pin org
  :defer t)

;; LaTeX export
(use-package ox-latex
  :defer t
  :config
  (progn
    (setq org-latex-pdf-process
          (-repeat 3 "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"))
    (add-to-list 'org-latex-packages-alist '("" "minted"))
    (setq org-latex-listings 'minted)))

;; Integration with beamer
(use-package ox-beamer
  :defer t
  :config
  (progn
    ;; Don't ask me if this variable can be evaluated.
    (put 'org-beamer-outline-frame-title 'safe-local-variable 'stringp)
    (add-to-list 'org-beamer-environments-extra
                 '("onlyenv+block"
                   "O"
                   "\\begin{onlyenv}%a\\begin{block}{%h}"
                   "\\end{block}\\end{onlyenv}"))))

(use-package org-journal
  :ensure t
  :defer t
  :mode
  ("journal/[0-9]\\{8\\}$" . org-journal-mode)
  :config
  (setq org-journal-dir (concat org-directory "/journal/")))

(use-package org-bullets
  :ensure t
  :defer t
  :init
  (add-hook #'org-mode-hook #'org-bullets-mode))

(use-package ox-reveal
  :ensure t
  :defer t)

(provide 'setup-org-mode)
