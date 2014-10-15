(require 'org)
(require 'dash)
(require 's)
(require 'f)

;;;###autoload
(defun org-list-tags-directory-files (directory)
  "Create org-table of tags from files in DIRECTORY.

The table is sorted by tag occurrence."
  (interactive
   (list
    (ido-read-directory-name "Org tags for files in: ")))
  (let* ((files (f-entries directory))
         (all-tags (-mapcat 'org-get-tags-file files))
         (grouped-tags (-group-by 'identity all-tags))
         (tags-occurrence (--map (list (car it) (length (cdr it)))
                                grouped-tags))
         (sorted-tags-occurrence (--sort (> (cadr it) (cadr other))
                                        tags-occurrence))
         (data (--map (list (car it) (number-to-string (cadr it)))
                      sorted-tags-occurrence)))
    (pop-to-buffer
     (get-buffer-create
      (format "*org-tags-%s*" (file-name-directory directory))))
    (erase-buffer)
    (org-mode)
    (org-table-insert-from-list '("Tag" "Occurrence") data)
    (org-table-align)))

(defun org-get-tags-file (file)
  "Get all tags in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (-map 'car (org-get-buffer-tags))))

(defun org-table-insert-from-list (header data)
  "Insert table with HEADER, hline and DATA."
  (org-table-insert-row-from-list header)
  (forward-line -1)
  (org-table-insert-hline)
  (forward-line 2)
  (-each data 'org-table-insert-row-from-list)
  (forward-line -1)
  (org-table-insert-hline))

(defun org-table-insert-row-from-list (row)
  "Insert org-table row based on ROW."
  (insert "|" (s-join "|" row) "|\n"))
