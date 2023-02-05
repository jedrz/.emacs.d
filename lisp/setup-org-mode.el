;;  -*- lexical-binding: t; -*-

;; Configuration based on https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html

(defvar my-org-base-agenda-files '("~/Dokumenty/org/gtd/inbox.org"
                                   "~/Dokumenty/org/gtd/gtd.org"
                                   "~/Dokumenty/org/gtd/tickler.org"
                                   "~/Dokumenty/org/gtd/gcal.org"))

(use-package org
  :ensure t
  :defer t
  :bind
  (("C-c a" . org-agenda)
   ("C-c l" . org-store-link)
   ("C-c b" . org-switchb)
   ("C-c k" . org-capture))
  :config
  ;; Indent (view only) headlines and text.
  (setq org-startup-indented t)

  ;; Single key navigation for headlines.
  (setq org-use-speed-commands t)

  ;; Set up paths.
  (setq org-directory "~/Dokumenty/org"
        org-agenda-files my-org-base-agenda-files
        org-default-notes-file "~/Dokumenty/org/gtd/inbox.org")

  (setq org-todo-keywords
        '((sequence "TODO(t!)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")))

  (setq org-log-into-drawer "LOGBOOK")

  (setq org-todo-keyword-faces
        '(("WAITING" :foreground "orange" :weight bold)
          ("CANCELLED" :foreground "forest green" :weight bold)))

  (setq org-capture-templates
        '(("t" "Todo [inbox]" entry
           (file "~/Dokumenty/org/gtd/inbox.org")
           "* TODO %i%?")
          ("T" "Tickler" entry
           (file+headline "~/Dokumenty/org/gtd/tickler.org" "Tickler")
           "* TODO %i%?\n\n%^t\n\n")
          ("a" "Appointment" entry
           (file  "~/Dokumenty/org/gtd/gcal.org")
           "* %?\n\n%^T\n")
          ("A" "Appointment [Day]" entry
           (file  "~/Dokumenty/org/gtd/gcal.org")
           "* %?\n\n%^t\n")))

  (setq org-refile-targets '((org-agenda-files :maxlevel . 3))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)

  (setq org-agenda-custom-commands
        '((" " "Agenda"
           ((tags
             "REFILE"
             ((org-agenda-overriding-header "To refile")
              (org-tags-match-list-sublevels nil)))))))

  ;; Do not split line when cursor in not at the end.
  (setq org-M-RET-may-split-line nil)

  ;; Highlight source code.
  (setq org-src-fontify-natively t)

  ;; Add a timestamp when a certain TODO item was finished.
  (setq org-log-done 'time)

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

  ;; Autosave org mode buffers.
  (add-hook 'auto-save-hook 'org-save-all-org-buffers)

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
  (setq org-export-babel-evaluate nil))

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
  :init
  (setq org-journal-dir "~/Dokumenty/org/journal/"))

(use-package org-superstar
  :ensure t
  :defer t
  :init
  (add-hook #'org-mode-hook #'org-superstar-mode))

(use-package ox-reveal
  :ensure t
  :defer t)

(use-package orgtbl-aggregate
  :ensure t
  :defer t)

(use-package org-gcal
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'org-agenda-mode-hook #'org-gcal-sync)
    (add-hook 'org-capture-after-finalize-hook #'org-gcal-sync))
  :config
  (progn
    (load "setup-org-mode-private.el")
    (setq org-gcal-down-days 730)))

(use-package org-tree-slide
  :ensure t
  :defer t
  :config
  (setq org-tree-slide-cursor-init nil)
  :bind (:map org-mode-map ("<f8>" . org-tree-slide-mode)))

;; Based on:
;; https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/
;; https://d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5.html
(use-package org-roam
  :ensure t
  :defer t
  :custom
  (org-roam-directory "~/Dokumenty/org/roam")
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("b" "book notes" plain
      "\n* Pozycja\n\nAutor: %^{Autor}\nTytuł: ${title}\nRok: %^{Rok}\n\n* Podsumowanie\n\n%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("p" "project" plain "* Zadania\n\n** TODO Pierwsze zadanie\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}")
      :unnarrowed t)))
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert))
  :init
  (add-hook 'find-file-hook #'my-org-roam-project-update-tag)
  (add-hook 'before-save-hook #'my-org-roam-project-update-tag)
  (advice-add 'org-agenda :before #'my-org-roam-agenda-files-update)
  (advice-add 'org-todo-list :before #'my-org-roam-agenda-files-update)
  :config
  (require 'vulpea)

  (defun my-org-roam-project-p ()
    "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
    (org-element-map
        (org-element-parse-buffer 'headline)
        'headline
      (lambda (h)
        (eq (org-element-property :todo-type h)
            'todo))
      nil 'first-match))

  (defun my-org-roam-project-files ()
    "Return a list of note files containing 'project' tag." ;
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
                :from tags
                :left-join nodes
                :on (= tags:node-id nodes:id)
                :where (like tag (quote "%\"project\"%"))]))))

  (defun my-org-roam-project-update-tag ()
    "Update PROJECT tag in the current buffer."
    (when (and (not (active-minibuffer-window))
               (my-org-roam-buffer-p))
      (save-excursion
        (goto-char (point-min))
        (let* ((tags (vulpea-buffer-tags-get))
               (original-tags tags))
          (if (my-org-roam-project-p)
              (setq tags (cons "project" tags))
            (setq tags (remove "project" tags)))

          ;; cleanup duplicates
          (setq tags (seq-uniq tags))

          ;; update tags if changed
          (when (or (seq-difference tags original-tags)
                    (seq-difference original-tags tags))
            (apply #'vulpea-buffer-tags-set tags))))))

  (defun my-org-roam-buffer-p ()
    "Return non-nil if the currently visited buffer is a note."
    (and buffer-file-name
         (string-prefix-p
          (expand-file-name (file-name-as-directory org-roam-directory))
          (file-name-directory buffer-file-name))))

  (defun my-org-roam-agenda-files-update (&rest _)
    "Update the value of `org-agenda-files'."
    (setq org-agenda-files (append my-org-base-agenda-files
                                   (my-org-roam-project-files))))

  (org-roam-db-autosync-mode))

(use-package vulpea
  :ensure t
  :defer t)

(provide 'setup-org-mode)
