;; DONE support a filter
;; TODO use one interactive function for changes and one for topics

(defun gerrit-section--filter (message-info)
  (or (s-starts-with? "jenkins" (alist-get 'name (alist-get 'author message-info)))
      (s-ends-with? "/verify" (alist-get 'message message-info))))


(defun gerrit-section-insert-comments (topicname)
  (magit-insert-section (toplevel) ; without this, I can't toggle the repo
                                        ; sections for some reason.

    (insert "This is some text that describes what this change is about\n")

    (cl-loop for change-info in (gerrit-rest-get-topic-info topicname) do
             (let ((changenr (alist-get '_number change-info))
                   (project (alist-get 'project change-info)))
               (magit-insert-section (change1 changenr)
                 ;; projectname: First line of commit msg, maybe owner
                 (magit-insert-heading (format "%s: %s:" project changenr))
                 (cl-loop for message-info in (cl-remove-if #'gerrit-section--filter
                                                            (gerrit-rest-change-get-messages
                                                             changenr))
                          do
                          (let* ((name (alist-get 'name (alist-get 'author message-info)))
                                 (date (gerrit--format-abbrev-date (alist-get 'date message-info)))
                                 (msg (s-lines (alist-get 'message message-info)))
                                 ;; I'm sure firstline and restlines can be determined in a better way
                                 (firstline (car msg))
                                 (restlines (s-join "\n" (cdr msg)))
                                 ;; (available-width (- (window-width) (length name) 2))
                                 (available-width 80)
                                 ;; (section-suffix-fmt (format " %%-%ds %%10s\n" (- available-width 10)))
                                 )
                            (magit-insert-section (gerrit-comments)

                              (magit-insert-heading
                                (format "%-17s %-40s %10s" ;; authorname, first line of messsage, date
                                        (propertize name 'font-lock-face
                                                    'magit-section-secondary-heading)
                                        ;; replace some strings in firstline: e.g.
                                        ;; DONE Patch Set \d+ -> PS\d+
                                        ;; DONE Code-Review:+2 -> (propertize "✔" 'face 'gerrit-success)
                                        ;; (if (stringp firstline)
                                        ;;     (s-replace-all
                                        ;;      '(
                                        ;;        ;; ("Patch Set " . "PS")
                                        ;;        ;; ("Code-Review+2" . (propertize "✔" 'face 'gerrit-success))
                                        ;;        ;; ("Code-Review+1" . (propertize "+1" 'face 'gerrit-success))
                                        ;;        ;; ("Code-Review-1" . (propertize "-1" 'face 'gerrit-fail))
                                        ;;        ;; ("Code-Review-2" . (propertize "-2" 'face 'gerrit-fail)))
                                        ;;        )
                                        ;;      ;;  '(("Patch Set " . "PS"))
                                        ;;      ;; (propertize firstline 'face 'gerrit-fail))
                                        ;;      firstline
                                        ;;      )
                                        ;;   "")
                                        (propertize firstline 'face 'gerrit-fail)

                                        (propertize date 'face 'magit-log-date)
                                        ))

                              ;; only show the rest of the message if the message is multiline
                              (unless (s-blank? restlines)
                                (insert (concat restlines "\n")))
                              ))))))
    (insert ?\n)))

(defun gerrit-sec-fmt ()
  (interactive)
  (let* ((left "firstname.surname")
         (available-width (- (window-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left "10days ago")))

(defun gerrit-section-demo (topicname)
  (interactive "sEnter topicname: ")
  (let ((buffer (get-buffer-create "*gerrit-section*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (magit-section-mode)
        (gerrit-section-insert-comments topicname)))))
