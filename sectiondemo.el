(defun gerrit-section-insert-comments ()
  (magit-insert-section (toplevel) ; without this, I can't toggle the repo
                                        ; sections for some reason.

    (insert "This is some text that describes what this change is about\n")

    (cl-loop for sectionname in '("authorname1" "authorname2" "a3") do
             (magit-insert-section (gerrit-comments)
               ;; (magit-insert-heading (concat sectionname ":"))
               (magit-insert-heading sectionname)

               ;; copied from magit-insert-child-count
               (save-excursion
                 (goto-char (1- (point)))
                 ;; (goto-char (- (point) (length sectionname)))
                 ;; (goto-char (- (point) (length sectionname)))
                 ;; (beginning-of-line)
                 ;; first line
                 ;; TODO align the first lines
                 (insert " PS1\n")
                 (delete-char 1)
                 ;; (delete-char (length sectionname))
                 )

               ;; (insert "sec details")
               (magit-insert-section (gerrit-comment (concat "file:" sectionname))
                 (magit-insert-heading "comment1")
                 (insert "text text text\n")

                 )))
    (insert ?\n)))


;; (add-hook 'magit-status-sections-hook #'gerrit-section-insert-comments t)


(defun gerrit-section-demo ()
  (interactive)
  (let ((buffer (get-buffer-create "*gerrit-section*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (magit-section-mode)
        (gerrit-section-insert-comments)))))
