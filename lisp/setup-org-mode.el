;; Configuration based on https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html

(use-package org
  :ensure t
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

    ;; Set up paths.
    (setq org-directory "~/Dokumenty/org"
          org-agenda-files '("~/Dokumenty/org/gtd/inbox.org"
                             "~/Dokumenty/org/gtd/gtd.org"
                             "~/Dokumenty/org/gtd/tickler.org")
          org-default-notes-file "~/Dokumenty/org/gtd/inbox.org")

    (setq org-todo-keywords
          '((sequence "TODO(t!)" "WAITING(w!)" "|" "DONE(d!)" "CANCELLED(c!)")))

    (setq org-todo-keyword-faces
          '(("WAITING" :foreground "orange" :weight bold)
            ("CANCELLED" :foreground "forest green" :weight bold)))

    (setq org-capture-templates
          '(("t" "Todo [inbox]" entry
             (file+headline "~/Dokumenty/org/gtd/inbox.org" "Tasks")
             "* TODO %i%?")
            ("T" "Tickler" entry
             (file+headline "~/Dokumenty/org/gtd/tickler.org" "Tickler")
             "* %i%? \n %U")))

    (setq org-refile-targets
          '(("~/Dokumenty/org/gtd/gtd.org" :maxlevel . 3)
            ("~/Dokumenty/org/gtd/someday.org" :level . 1)
            ("~/Dokumenty/org/gtd/tickler.org" :maxlevel . 2))
          org-refile-use-outline-path 'file)

    (setq org-agenda-custom-commands
          '(("g" "Getting Things Done"
             ((agenda "")
              (todo
               ""
               ((org-agenda-overriding-header "Next action")
                (org-agenda-skip-function
                 '(or (my-org-agenda-skip-all-siblings-but-first)
                      (my-org-agenda-skip-file "tickler.org")
                      (my-org-agenda-skip-file "inbox.org")))
                (org-agenda-prefix-format "  ")))
              (todo
               ""
               ((org-agenda-overriding-header "All")
                (org-agenda-skip-function
                 '(my-org-agenda-skip-file "tickler.org"))
                (org-agenda-prefix-format "  ")))))))

    (defun my-org-agenda-skip-all-siblings-but-first ()
      "Skip all but the first non-done entry."
      (let (should-skip-entry)
        (unless (org-current-is-todo)
          (setq should-skip-entry t))
        (save-excursion
          (while (and (not should-skip-entry) (org-goto-sibling t))
            (when (org-current-is-todo)
              (setq should-skip-entry t))))
        (when should-skip-entry
          (or (outline-next-heading)
              (goto-char (point-max))))))

    (defun my-org-agenda-skip-file (filename)
      (when (string-suffix-p filename (buffer-file-name))
        (point-max)))

    (defun org-current-is-todo ()
      (string= "TODO" (org-get-todo-state)))

    ;; Log state changes into the LOGBOOK.
    (setq org-log-into-drawer t)

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
       (shell . t)
       (python . t)
       ;; (R . t)
       (calc . t)
       (sql . t)))

    ;; Never evaluate blocks when exporting.
    (setq org-export-babel-evaluate nil)))

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
