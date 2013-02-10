(require 'popup)

;;;###autoload
(defun yas-popup-prompt (prompt choices &optional display-fn)
  "Select a snippet with popup library."
  (popup-menu*
   (mapcar (lambda (choice)
             (popup-make-item
              (or (and display-fn (funcall display-fn choice))
                  choice)
              :value choice))
           choices)
   :prompt prompt))
