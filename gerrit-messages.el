(defun gerrit-section-insert-comments ()
  (magit-insert-section (toplevel) ; without this, I can't toggle the repo
                                   ; sections for some reason.

    (insert "This is some text that describes what this change is about\n")

    (cl-loop for message-info in (gerrit-rest-change-get-messages "Ifa28f572c50a8e6cd894c98bb6ea58babb6c7b0a") do
             (let* ((name (alist-get 'name (alist-get 'author message-info)))
                    (date (gerrit--format-abbrev-date (alist-get 'date message-info)))
                    (msg (s-lines (alist-get 'message message-info)))
                    (firstline (car msg))
                    (restlines (s-join "\n" (cdr msg)))
                    (available-width (- (window-width) (length name) 2))
                    (section-suffix-fmt (format " %%-%ds %%10s\n" (- available-width 10))))
               (magit-insert-section (gerrit-comments)
                 (magit-insert-heading name)

                 ;; copied from magit-insert-child-count
                 (save-excursion
                   (goto-char (1- (point)))
                   (insert (format section-suffix-fmt firstline date))
                   (delete-char 1)
                   )

                 (unless (s-blank? restlines)
                   (insert (concat restlines "\n")))
                 )))
    (insert ?\n)))

(defun gerrit-sec-fmt ()
  (interactive)
  (let* ((left "firstname.surname")
         (available-width (- (window-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left "10days ago")))

(defun gerrit-section-demo ()
  (interactive)
  (let ((buffer (get-buffer-create "*gerrit-section*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (magit-section-mode)
        (gerrit-section-insert-comments)))))
