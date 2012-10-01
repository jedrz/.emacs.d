;;; Commentary
;;
;; Author: Scott Frazer
;; http://scottfrazersblog.blogspot.com/2009/12/emacs-named-desktop-sessions.html
;;
;; Keep all your sessions in one directory.
;;
;; my-desktop-save -- Save the current session by name
;; my-desktop-save-and-clear -- Same as above, but clear out all the buffers so you start with a "clean" session
;; my-desktop-read -- Load a session by name
;; my-desktop-change -- Save the current session and load a different one
;; my-desktop-name -- Echo the current session name
;;
;; My modification:
;; my-desktop-remove -- Remove chosen session

;;; Code:

(require 'desktop)

(defvar my-desktop-session-dir
  (concat user-emacs-directory "desktop-sessions/")
  "Directory to save desktop sessions in")

(defvar my-desktop-session-name-hist nil
  "Desktop session name history")

(defun my-desktop-save (&optional name)
  "Save desktop by name."
  (interactive)
  (unless name
    (setq name (my-desktop-get-session-name "Save session" t)))
  (when name
    (make-directory (concat my-desktop-session-dir name) t)
    (desktop-save (concat my-desktop-session-dir name) t)))

(defun my-desktop-save-and-clear ()
  "Save and clear desktop."
  (interactive)
  (call-interactively 'my-desktop-save)
  (desktop-clear)
  (setq desktop-dirname nil))

(defun my-desktop-read (&optional name)
  "Read desktop by name."
  (interactive)
  (unless name
    (setq name (my-desktop-get-session-name "Load session")))
  (when name
    (desktop-clear)
    (desktop-read (concat my-desktop-session-dir name))))

(defun my-desktop-change (&optional name)
  "Change desktops by name."
  (interactive)
  (let ((name (my-desktop-get-current-name)))
    (when name
      (my-desktop-save name))
    (call-interactively 'my-desktop-read)))

(defun my-desktop-remove (&optional name)
  "Remove desktop from disk by name."
  (interactive)
  (unless name
    (setq name (my-desktop-get-session-name "Remove session" t)))
  (let ((desktop-to-delete-path (concat my-desktop-session-dir name)))
   (if (string= name (my-desktop-get-current-name))
       ;; If session to delete is current session then let
       ;; `desktop-remove' function set `desktop-dirname' variable to nil.
       (desktop-remove)
     ;; If other session will be deleted
     ;; then preserve `desktop-dirname' value.
     (let ((desktop-dirname desktop-to-delete-path))
       (desktop-remove)))
   ;; Delete the .lock file
   (delete-file (concat (file-name-as-directory desktop-to-delete-path)
                        (concat desktop-base-file-name ".lock")))
   ;; Now delete the empty directory.
   (delete-directory desktop-to-delete-path)))

(defun my-desktop-name ()
  "Return the current desktop name."
  (interactive)
  (let ((name (my-desktop-get-current-name)))
    (if name
        (message (concat "Desktop name: " name))
      (message "No named desktop loaded"))))

(defun my-desktop-get-current-name ()
  "Get the current desktop name."
  (when desktop-dirname
    (let ((dirname (substring desktop-dirname 0 -1)))
      (when (string= (file-name-directory dirname) my-desktop-session-dir)
        (file-name-nondirectory dirname)))))

(defun my-desktop-get-session-name (prompt &optional use-default)
  "Get a session name."
  (let* ((default (and use-default (my-desktop-get-current-name)))
         (full-prompt (concat prompt (if default
                                         (concat " (default " default "): ")
                                       ": "))))
    (completing-read full-prompt (and (file-exists-p my-desktop-session-dir)
                                      (directory-files my-desktop-session-dir))
                     nil nil nil my-desktop-session-name-hist default)))

(defun my-desktop-kill-emacs-hook ()
  "Save desktop before killing emacs."
  ;; Save also current session.
  (let ((current-desktop-name (my-desktop-get-current-name)))
    (when current-desktop-name
      (my-desktop-save current-desktop-name)))
  (when (file-exists-p (concat my-desktop-session-dir "last-session"))
    (setq desktop-file-modtime
          (nth 5 (file-attributes (desktop-full-file-name (concat my-desktop-session-dir "last-session"))))))
  (my-desktop-save "last-session"))

(add-hook 'kill-emacs-hook 'my-desktop-kill-emacs-hook)

(provide 'my-desktop)
