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
(require 'gerrit-rest)

(defun gerrit-section-filter (message-info)
  "Filter function run for every gerrit comment.

This function is called for every comment MESSAGE-INFO
of a gerrit change.  If the function returns t, the comment is
shown in the section buffer."
  ;; If you don't want to see messages from jenkins or comments that start
  ;; with /verify use this code:
  ;; (not (or (s-starts-with? "jenkins" (alist-get 'name (alist-get 'author message-info)))
  ;;     (s-ends-with? "/verify" (alist-get 'message message-info)))))
  t)

(defun gerrit-section--insert-change-comments (change-info)
  (let ((changenr (alist-get '_number change-info))
        (project (alist-get 'project change-info))
        (subject (alist-get 'subject change-info))
        (latest-commit-message
         (let* ((revisions (alist-get 'revisions change-info))
                (revision (alist-get 'current_revision change-info)))
           (gerrit--alist-get-recursive (intern revision) 'commit 'message revisions)))
        (comment-fmt (format "%%-17s %%-%ds %%10s" (- (window-width) 17 10 2))))
    (magit-insert-section (gerrit-change changenr)
      ;; projectname: First line of commit msg, maybe owner
      (magit-insert-heading
        (propertize (format "[%s] " changenr) 'face 'magit-hash)
        (concat project " ")
        (propertize subject 'face 'magit-section-heading))

      ;; maybe when-let can be removed, since all commit
      ;; messages are multiline (they include a Change-Id line)
      (when-let (commit-restlines (cdr (s-split-up-to "\n" latest-commit-message 1)))
        (insert (s-trim-left (s-join "\n" commit-restlines))))

      (cl-loop for message-info in (cl-remove-if-not #'gerrit-section-filter
                                                     (gerrit-rest-change-get-messages
                                                      changenr))
               do
               (let* ((name (alist-get 'name (alist-get 'author message-info)))
                      (date (gerrit--format-abbrev-date (alist-get 'date message-info)))
                      (msg (s-lines (alist-get 'message message-info)))
                      ;; I'm sure firstline and restlines can be determined in a better way
                      (firstline (car msg))
                      (restlines (s-join "\n" (cdr msg))))
                 (magit-insert-section (gerrit-comments)

                   (magit-insert-heading
                     (format comment-fmt ;; authorname, first line of messsage, date
                             (propertize name 'font-lock-face
                                         'magit-section-secondary-heading)
                             ;; replace some strings in firstline: e.g.
                             ;; DONE Patch Set \d+ -> PS\d+
                             ;; DONE Code-Review:+2 -> (propertize "✔" 'face 'gerrit-success)

                             ;;

                             ;; replace-match((propertize "+1" 'face 'gerrit-success) t t #("Code-Review+1" 0 13 (face gerrit-fail)) nil)
                             ;; replace-regexp-in-string("\\(?:Code-Review\\(?:\\+[12]\\|-[12]\\)\\|\\(?:Patch S\\|patch s\\)et \\)" #f(compiled-function (it) #<bytecode 0x1e0332ac5e2b>) #("Patch Set 1: Code-Review+1" 0 26 (face gerrit-fail)) t t)

                             ;; (s-replace-all
                             ;;  '(
                             ;;    ("patch set " . "PS")
                             ;;    ("Patch Set " . "PS")
                             ;;    ("Code-Review+2" . (propertize "✔" 'face 'gerrit-success))
                             ;;    ("Code-Review-1" . (propertize "-1" 'face 'gerrit-fail))
                             ;;    ("Code-Review+1" . (propertize "+1" 'face 'gerrit-success))
                             ;;    ("Code-Review-2" . (propertize "-2" 'face 'gerrit-fail))
                             ;;    )
                             ;;  (propertize firstline 'face 'gerrit-fail))
                             ;; TODO use the above version (once the bug in s.el is fixed)
                             (s-replace "Verified-1" (propertize "VR-1" 'face 'gerrit-fail)
                                        (s-replace "Verified+1" (propertize "VR+1" 'face 'gerrit-success)
                                                   (s-replace "Code-Review+1" (propertize "CR+1" 'face 'gerrit-success)
                                                              (s-replace
                                                               "Code-Review+2" (propertize "CR✔" 'face 'gerrit-success)
                                                               (s-replace "Patch Set " "PS" firstline)))))

                             (propertize date 'face 'magit-log-date)
                             ))

                   ;; only show the rest of the message if the message is multi-line
                   (unless (s-blank? restlines)
                     (insert (concat restlines "\n")))))))))

(defun gerrit-section--info (topicname changenr)
  (with-current-buffer (get-buffer-create "*gerrit-section*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (magit-section-mode)
      ;; without this top-level section, I can't toggle the repo sections for some reason.
      (magit-insert-section (toplevel)
        (when changenr
          (gerrit-section--insert-change-comments (gerrit-rest-get-change-info changenr)))
        (when topicname
          ;; TODO check if there are no open topics with the name
          ;; topicname. If this is the case, check if there are non-open
          ;; topics with this name and offer to open it.
          (cl-loop for change-info in (gerrit-rest-get-topic-info topicname) do
                   (gerrit-section--insert-change-comments change-info)))
        (insert ?\n)))
    (switch-to-buffer-other-window (current-buffer))))

(defun gerrit-section-topic-info (topicname)
  (interactive "sEnter topicname: ")
  (gerrit-section--info topicname nil))

(defun gerrit-section-change-info (changenr)
  (interactive "sEnter changenr: ")
  (gerrit-section--info nil changenr))

(provide 'gerrit-messages)
;;; gerrit-messages.el ends here
