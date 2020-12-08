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
         (let ((revisions (alist-get 'revisions change-info))
                (revision (alist-get 'current_revision change-info)))
           (gerrit--alist-get-recursive (intern revision) 'commit 'message revisions)))
        (comment-fmt (let ((max-author-width 17)
                           (max-date-width 10))
                         (format "%%-%ds %%-%ds %%%ds"
                             max-author-width
                             (- (window-width) max-author-width max-date-width 2)
                             max-date-width
                             ))))
    (magit-insert-section (gerrit-change changenr)
      ;; projectname: First line of commit msg, maybe owner
      (magit-insert-heading
        (propertize (format "[%s] " changenr) 'font-lock-face 'magit-hash)
        (concat project " ")
        (propertize subject 'font-lock-face 'magit-section-heading))

      ;; maybe when-let can be removed, since all commit
      ;; messages are multiline (they include a Change-Id line)
      (when-let (commit-msg-body (cdr (s-split-up-to "\n" latest-commit-message 1)))
        (insert (s-trim-left (s-join "\n" commit-msg-body))))

      (cl-loop for message-info in (cl-remove-if-not #'gerrit-section-filter
                                                     (gerrit-rest-change-get-messages
                                                      changenr))
               do
               (let* ((name (alist-get 'name (alist-get 'author message-info)))
                      (date (gerrit--format-abbrev-date (alist-get 'date message-info)))
                      (msg (s-lines (alist-get 'message message-info)))
                      ;; I'm sure firstline and restlines can be determined in a better way
                      (firstline (car msg))
                      (restlines (s-join "\n" (cdr msg)))
                      (heading (format comment-fmt ;; authorname, first line of message, date
                                       (propertize name 'font-lock-face
                                                   'magit-section-secondary-heading)
                                       (s-replace  "Patch Set " "PS" firstline)
                                       (propertize date 'font-lock-face 'magit-log-date))))

                 (magit-insert-section (gerrit-comments)
                     (let ((boundary 0))
                       (when (string-match "Code-Review\[+-\]\[12\]" heading boundary)
                         (setq boundary (match-end 0))
                         (magit--put-face (match-beginning 0) boundary
                                          (if (s-match ".*\\+\[12\]$" (match-string-no-properties 0 heading))
                                              'magit-diff-added-highlight
                                            'magit-diff-removed-highlight)
                                          heading))
                       (setq boundary 0)
                       (when (string-match "Verified\[+-\]\[12\]" heading boundary)
                         (setq boundary (match-end 0))
                         (magit--put-face (match-beginning 0) boundary
                                          (if (s-match ".*\\+\[1\]$" (match-string-no-properties 0 heading))
                                              'magit-diff-added-highlight
                                            'magit-diff-removed-highlight)
                                          heading)))

                     (magit-insert-heading heading)

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
