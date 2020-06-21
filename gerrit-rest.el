;;; gerrit-rest.el --- REST layer of gerrit.el -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Thomas Hisch <t.hisch@gmail.com>
;;
;; Author: Thomas Hisch <t.hisch@gmail.com>
;; Version: 0.1
;; URL: https://github.com/thisch/gerrit.el
;; Package-Requires: ((emacs "25.1") (hydra "0.15.0") (magit "2.13.1") (s "1.12.0"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; helper functions using the REST API of gerrit

;;; Code:

(eval-when-compile (require 'subr-x)) ;; when-let
(require 's)
(require 'json)
(require 'cl-lib)

(defvar gerrit-host)

(defcustom gerrit-rest-endpoint-prefix "/a"
  "String that is appended to 'gerrit-host`.
For newer gerrit servers this needs to be set to /a, whereas on older
servers it needs to be set to an empty string."
  :group 'gerrit
  :type 'str)

(defvar gerrit-rest-api-debug-flag nil
  "Non-nil means enable debugging of problems with the rest API of gerrit.")

(defun gerrit-rest-authentication ()
  "Return an encoded string with gerrit username and password."
  (let ((pass-entry (auth-source-user-and-password gerrit-host)))
    (when-let ((username (nth 0 pass-entry))
               (password (nth 1 pass-entry)))
      (base64-encode-string
       (concat username ":" password)))))

(defun gerrit-rest-toggle-api-debug-flag ()
  "Toggle the internal debug flag."
  (interactive)
  (setq gerrit-rest-api-debug-flag (not gerrit-rest-api-debug-flag))
  (message "set gerrit-rest debug flag to '%s'" gerrit-rest-api-debug-flag))

(defun gerrit-rest-sync (method data &optional path)
  "Interact with the API using method METHOD and data DATA.
Optional arg PATH may be provided to specify another location further
down the URL structure to send the request."
  (let ((url-request-method method)
        (url-request-extra-headers
         `(("Content-Type" . "application/json")
           ("Authorization" . ,(concat "Basic " (gerrit-rest-authentication)))))
        (url-request-data data)
        (target (concat "https://" gerrit-host gerrit-rest-endpoint-prefix path)))

    (if (not gerrit-rest-api-debug-flag)
        (with-current-buffer (url-retrieve-synchronously target t)
          (json-read-from-string
           (progn
             (goto-char (point-min))
             ;; if there is an error in search-forward-regexp, write
             ;; the buffer contents to a *gerrit-rest-status* buffer
             (if-let ((pos (search-forward-regexp (concat "^" (regexp-quote ")]}'") "$") nil t)))
                 (buffer-substring pos (point-max))
                 ;; ")]}'" was not found in the REST response
                 (let ((buffer (get-buffer-create "*gerrit-rest-status*"))
                       (contents (buffer-substring (point-min) (point-max))))
                   (with-current-buffer buffer
                     (goto-char (point-max))
                     (insert ?\n)
                     (insert (format "%s: %s (%s)" url-request-method target url-request-extra-headers))
                     (insert ?\n)
                     (insert contents)
                     (error (concat "error with gerrit request (take a look at the "
                                    "*gerrit-rest-status* buffer for more information"))))))))
      (progn
        ;; TODO improve this, fontify json data?
        (switch-to-buffer (url-retrieve-synchronously target))
        (goto-char (point-min))
        (insert target)
        (insert ?\n)))))

(defun gerrit-rest--escape-project (project)
  "Escape project name PROJECT for usage in REST API requets."
  (s-replace-all '(("/" . "%2F")) project))

(defun gerrit-rest-get-server-version ()
  "Return the gerrit server version."
  (interactive)
  (gerrit-rest-sync "GET" nil "/config/server/version"))

(defun gerrit-rest-get-server-info ()
  "Return the gerrit server info."
  (interactive)
  (gerrit-rest-sync "GET" nil "/config/server/info"))

(defun gerrit-rest-get-topic-info (topicname)
  "Return information about an open topic with TOPICNAME."
  ;; TODO create new buffer and insert stuff there
  ;; TODO query open topics
  (interactive "sEnter a topic name: ")
  (let* ((fmtstr (concat "/changes/?q=is:open+topic:%s&"
                         "o=DOWNLOAD_COMMANDS&"
                         "o=CURRENT_REVISION&"
                         "o=CURRENT_COMMIT&"
                         "o=DETAILED_LABELS&"
                         "o=DETAILED_ACCOUNTS"))
         (json-array-type 'list)
         (req (format fmtstr topicname)))
    (gerrit-rest-sync "GET" nil req)))

(defun gerrit-rest--get-gerrit-accounts ()
  "Return an alist of all active gerrit users."
  (interactive)
  (condition-case nil
      (mapcar (lambda (account-info) (cons (cdr (assoc '_account_id account-info))
                                      (cdr (assoc 'username account-info))))
              (let ((json-array-type 'list))
                ;; see https://gerrit-review.googlesource.com/Documentation/rest-api-accounts.html
                ;; and https://gerrit-review.googlesource.com/Documentation/user-search-accounts.html#_search_operators
                (gerrit-rest-sync "GET" nil "/accounts/?q=is:active&o=DETAILS&S=0")))
    (error '())))

(defun gerrit-rest--set-assignee (changenr assignee)
  "Set the assignee to ASSIGNEE of a change with nr CHANGENR."
  (interactive "sEnter a changenr: \nsEnter assignee: ")
  ;; TODO error handling?
  (gerrit-rest-sync "PUT"
                    (encode-coding-string (json-encode-list
                                           `((assignee . ,assignee))) 'utf-8)
                    (format "/changes/%s/assignee"  changenr)))

(defun gerrit-rest-open-reviews-for-project (project)
  "Return list of open reviews returned for the project PROJECT."
  (interactive "sEnter gerrit project: ")
  ;; see https://gerrit-review.googlesource.com/Documentation/rest-api-changes.html#list-changes
  (let* ((json-array-type 'list)
         (limit-entries 25)
         (req (format (concat "/changes/?q=is:open+project:%s&"
                              "o=CURRENT_REVISION&"
                              "o=CURRENT_COMMIT&"
                              "o=DETAILED_LABELS&"
                              (format "n=%d&" limit-entries)
                              "o=DETAILED_ACCOUNTS")
                      (funcall #'gerrit-rest--escape-project project)))
         (resp (gerrit-rest-sync "GET" nil req)))
    ;; (setq open-reviews-response resp) ;; for debugging only (use M-x ielm)
    resp))

(defun gerrit-rest-change-set-vote (changenr vote message)
  "Set a Code-Review vote VOTE of a change CHANGENR.
A comment MESSAGE can be provided."
  (interactive "sEnter a changenr: \nsEnter vote [-2, -1, 0, +1, +2]: \nsEnter message: ")
  (gerrit-rest-sync "POST"
                    (encode-coding-string (json-encode-list
                                           `((message . ,message)
                                             (labels .
                                               ((Code-Review . ,vote))))) 'utf-8)
                    (format "/changes/%s/revisions/current/review" changenr)))

(defun gerrit-rest-change-verify (changenr vote message)
  "Verify a change CHANGENR by voting with VOTE.
A comment MESSAGE can be provided."
  (interactive "sEnter a changenr: \nsEnter vote [-1, 0, +1]: \nsEnter message: ")
  (gerrit-rest-sync "POST"
                    (encode-coding-string (json-encode-list
                                           `((message . ,message)
                                             (labels .
                                               ((Code-Review . ,vote))))) 'utf-8)
                    (format "/changes/%s/revisions/current/review" changenr)))

(defun gerrit-rest-change-set-Work-in-Progress (changenr)
  "Set the state of the change CHANGENR to Work-in-Progress."
  (interactive "sEnter a changenr: ")
  (gerrit-rest-sync "POST"
                    (encode-coding-string (json-encode-list
                                           `((message . ,"Set using gerrit.el"))) 'utf-8)
                    (format "/changes/%s/wip" changenr)))

(defun gerrit-rest-change-set-Ready-for-Review (changenr)
  "Set the state of the change CHANGENR to Reday-for-Review."
  (interactive "sEnter a changenr: ")
  (gerrit-rest-sync "POST"
                    (encode-coding-string (json-encode-list
                                           `((message . ,"Set using gerrit.el"))) 'utf-8)
                    (format "/changes/%s/ready" changenr)))

(defun gerrit-rest-change-get-labels (changenr)
  "Returns the current labels dictionary of a change CHANGENR."
  (interactive "sEnter changenr: ")
  (let* ((req (format "/changes/%s/revisions/current/review" changenr))
         (json-array-type 'list)
         (resp (gerrit-rest-sync "GET" nil req)))
    (assoc 'labels (cdr resp))))

;;  topic commands

(defun gerrit-rest-topic-set-vote (topic vote message)
  "Set a Code-Review vote VOTE for all changes of a topic TOPIC.
A comment MESSAGE can be provided."
 (interactive "sEnter a topic: \nsEnter vote [-2, -1, 0, +1, +2]: \nsEnter message: ")
 (cl-loop for change-info in (gerrit-rest-get-topic-info topic) do
          (let ((changenr (cdr (assoc 'change_id (cdr change-info)))))
            (message "Setting vote %s for %s" vote changenr)
            (gerrit-rest-change-set-vote changenr vote message)
            )))

(defun gerrit-rest-topic-verify (topic vote message)
  "Verify a topic TOPIC by voting with VOTE.
A comment MESSAGE can be provided."
 (interactive "sEnter a topic: \nsEnter vote [-1, 0, +1]: \nsEnter message: ")
 (cl-loop for change-info in (gerrit-rest-get-topic-info topic) do
          (let ((changenr (cdr (assoc 'change_id (cdr change-info)))))
            (message "Setting Verify-vote %s for %s" vote changenr)
            (gerrit-rest-change-verify changenr vote message))))

(defun gerrit-rest-change-query (expression)
  "Return information about changes that match EXPRESSION."
  (interactive "sEnter a search expression: ")
  (let ((req (concat (format "/changes/?q=%s&" expression)
                     "o=CURRENT_REVISION&"
                     "o=CURRENT_COMMIT&"
                     "o=LABELS"
                     ;; "o=DETAILED_LABELS"
                     ;; "o=DETAILED_ACCOUNTS"))
                     ))
        (json-array-type 'list))
    (gerrit-rest-sync "GET" nil req)))

(provide 'gerrit-rest)

;;; gerrit-rest.el ends here
