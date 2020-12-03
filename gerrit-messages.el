;; DONE support a filter
;; DONE use one interactive function for changes and one for topics
;; TODO add support for reviewing entire topics in one buffer: gerrit-review-topic (topicname)
;;       take a look at the diffs of all changes
;;       allows adding comments to each change individually
;;       tree structure:
;;           [changenr] project: commit subject:
;;             commit msg body
;;             current patch:
;;                file A:
;;                file B:
;;             comments:
;;           [changenr] project: commit subject:
;;             commit msg body
;;             current patch:
;;                file C:
;;                file D:
;;             comments:

(defun gerrit-section--filter (message-info)
  (or (s-starts-with? "jenkins" (alist-get 'name (alist-get 'author message-info)))
      (s-ends-with? "/verify" (alist-get 'message message-info))))

(defun gerrit-section--insert-change-comments (change-info)
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

(defun gerrit-sec-fmt ()
  (interactive)
  (let* ((left "firstname.surname")
         (available-width (- (window-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left "10days ago")))

(defun gerrit-section--demo (topicname changenr)
  (let ((buffer (get-buffer-create "*gerrit-section*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (magit-section-mode)
        (magit-insert-section (toplevel) ; without this, I can't toggle the repo
                                        ; sections for some reason.
          (when changenr
            (gerrit-section--insert-change-comments (gerrit-rest-get-change-info changenr)))
          (when topicname
            (cl-loop for change-info in (gerrit-rest-get-topic-info topicname) do
                     (gerrit-section--insert-change-comments change-info)))
          (insert ?\n))))))

(defun gerrit-section-topic-demo (topicname)
  (interactive "sEnter topicname: ")
  (gerrit-section--demo topicname nil))

(defun gerrit-section-change-demo (changenr)
  (interactive "sEnter changenr: ")
  (gerrit-section--demo nil changenr))
