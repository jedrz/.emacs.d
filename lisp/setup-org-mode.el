;; Configuration based on http://doc.norang.ca/org-mode.html.

(use-package org
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

(with-eval-after-load 'ob-sql
  ;; Fix babel for postgresql.
  (when (version<= org-version "9")
    (eval-when-compile (require 'cl))
    (defun org-babel-execute:sql (body params)
      "Execute a block of Sql code with Babel.
This function is called by `org-babel-execute-src-block'."
      (let* ((result-params (cdr (assoc :result-params params)))
             (cmdline (cdr (assoc :cmdline params)))
             (dbhost (cdr (assoc :dbhost params)))
             (dbuser (cdr (assoc :dbuser params)))
             (dbpassword (cdr (assoc :dbpassword params)))
             (database (cdr (assoc :database params)))
             (engine (cdr (assoc :engine params)))
             (colnames-p (not (equal "no" (cdr (assoc :colnames params)))))
             (in-file (org-babel-temp-file "sql-in-"))
             (out-file (or (cdr (assoc :out-file params))
                           (org-babel-temp-file "sql-out-")))
             (header-delim "")
             (command (case (intern engine)
                            ('dbi (format "dbish --batch %s < %s | sed '%s' > %s"
                                          (or cmdline "")
                                          (org-babel-process-file-name in-file)
                                          "/^+/d;s/^|//;s/(NULL)/ /g;$d"
                                          (org-babel-process-file-name out-file)))
                            ('monetdb (format "mclient -f tab %s < %s > %s"
                                              (or cmdline "")
                                              (org-babel-process-file-name in-file)
                                              (org-babel-process-file-name out-file)))
                            ('msosql (format "osql %s -s \"\t\" -i %s -o %s"
                                             (or cmdline "")
                                             (org-babel-process-file-name in-file)
                                             (org-babel-process-file-name out-file)))
                            ('mysql (format "mysql %s %s %s < %s > %s"
                                            (dbstring-mysql dbhost dbuser dbpassword database)
                                            (if colnames-p "" "-N")
                                            (or cmdline "")
                                            (org-babel-process-file-name in-file)
                                            (org-babel-process-file-name out-file)))
                            ('postgresql (format
                                          "psql -A -P footer=off -F \"\t\"  -d %s -f %s -o %s %s"
                                          database
                                          (org-babel-process-file-name in-file)
                                          (org-babel-process-file-name out-file)
                                          (or cmdline "")))
                            (t (error "No support for the %s SQL engine" engine)))))
        (with-temp-file in-file
          (insert
           (case (intern engine)
                 ('dbi "/format partbox\n")
                 (t ""))
           (org-babel-expand-body:sql body params)))
        (message command)
        (org-babel-eval command "")
        (org-babel-result-cond result-params
          (with-temp-buffer
            (progn (insert-file-contents-literally out-file) (buffer-string)))
          (with-temp-buffer
            (cond
             ((or (eq (intern engine) 'mysql)
                  (eq (intern engine) 'dbi)
                  (eq (intern engine) 'postgresql))
              ;; Add header row delimiter after column-names header in first line
              (cond
               (colnames-p
                (with-temp-buffer
                  (insert-file-contents out-file)
                  (goto-char (point-min))
                  (forward-line 1)
                  (insert "-\n")
                  (setq header-delim "-")
                  (write-file out-file)))))
             (t
              ;; Need to figure out the delimiter for the header row
              (with-temp-buffer
                (insert-file-contents out-file)
                (goto-char (point-min))
                (when (re-search-forward "^\\(-+\\)[^-]" nil t)
                  (setq header-delim (match-string-no-properties 1)))
                (goto-char (point-max))
                (forward-char -1)
                (while (looking-at "\n")
                  (delete-char 1)
                  (goto-char (point-max))
                  (forward-char -1))
                (write-file out-file))))
            (org-table-import out-file '(16))
            (org-babel-reassemble-table
             (mapcar (lambda (x)
                       (if (string= (car x) header-delim)
                           'hline
                         x))
                     (org-table-to-lisp))
             (org-babel-pick-name (cdr (assoc :colname-names params))
                                  (cdr (assoc :colnames params)))
             (org-babel-pick-name (cdr (assoc :rowname-names params))
                                  (cdr (assoc :rownames params))))))))))

(provide 'setup-org-mode)
