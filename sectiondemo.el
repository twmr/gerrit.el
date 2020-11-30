(defun gerrit-section-insert-comments ()
  (magit-insert-section (toplevel) ; without this, I can't toggle the repo
                                   ; sections for some reason.
    (cl-loop for sectionname in '("sec1" "sec2" "sec3") do
             (magit-insert-section (gerrit-comments)
               ;; (magit-insert-heading (concat sectionname ":"))
               (magit-insert-heading sectionname)

               ;; copied from magit-insert-child-count
               (save-excursion
                 (goto-char (1- (point)))
                 ;; first line
                 (insert " PS1\n")
                 (delete-char 1))

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
