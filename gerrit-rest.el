;;; gerrit-rest.el --- REST layer of gerrit.el -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Thomas Hisch <t.hisch@gmail.com>
;;
;; Author: Thomas Hisch <t.hisch@gmail.com>
;; Version: 0.1
;; URL: https://github.com/thisch/gerrit.el
;; Package-Requires: ((emacs "25.1") (s "1.12.0"))

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

(require 's)

(defvar gerrit-host)

(defvar gerrit-rest-api-debug-flag nil
  "Non-nil means enable debugging of problems with the rest API of gerrit.")

(defun gerrit-rest-authentication ()
  "Return an encoded string with gerrit username and password."
  (let ((pass-entry (auth-source-user-and-password gerrit-host)))
    (if-let ((username (nth 0 pass-entry))
             (password (nth 1 pass-entry)))
        (base64-encode-string
         (concat username ":" password)))))

(defun gerrit-rest-toggle-api-debug-flag ()
  "Toggle the internal debug flag."
  (interactive)
  (setq gerrit-rest-api-debug-flag (not gerrit-rest-api-debug-flag))
  (message "set gerrit-rest debug flag to '%s'" gerrit-rest-api-debug-flag))

(defun gerrit-rest--encode-payload (payload)
  (and payload
       (progn
         (unless (stringp payload)
           (setq payload (json-encode-list payload)))
         (encode-coding-string payload 'utf-8))))

(defun gerrit-rest-sync (method data &optional path)
  "Interact with the API using method METHOD and data DATA.
Optional arg PATH may be provided to specify another location further
down the URL structure to send the request."
  (let ((url-request-method method)
        (url-request-extra-headers
         `(("Content-Type" . "application/json")
           ("Authorization" . ,(concat "Basic " (gerrit-rest-authentication)))))
        (url-request-data data)
        (target (concat "https://" gerrit-host "/a" path)))

    (if (not gerrit-rest-api-debug-flag)
        (with-current-buffer (url-retrieve-synchronously target t)
          (let ((resp (json-read-from-string
                       (progn
                         (goto-char (point-min))
                         (buffer-substring (search-forward-regexp
                                            (concat "^" (regexp-quote ")]}'") "$"))
                                           (point-max))))))

            resp))
      (progn
        ;; TODO improve this, fontify json data?
        (switch-to-buffer (url-retrieve-synchronously target))
        (goto-char (point-min))
        (insert target)
        (insert ?\n)))))

(defun gerrit-rest--escape-project (project)
  (s-replace-all '(("/" . "%2F")) project))

(defun gerrit-rest-get-server-version ()
  (interactive)
  (message (prin1-to-string (gerrit-rest-sync "GET" nil "/config/server/version"))))

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
         (req (format fmtstr topicname))
         (resp (gerrit-rest-sync "GET" nil req)))
    (message "%s" (prin1-to-string resp))))

(defun gerrit-rest--get-gerrit-usernames ()
  "Return a list of usernames of all active gerrit users."
  (interactive)
  (condition-case nil
      (mapcar (lambda (account-info) (cdr (assoc 'username (cdr account-info))))
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
                    (gerrit-rest--encode-payload `((assignee . ,assignee)))
                    (format "/changes/%s/assignee"  changenr)))

(defun gerrit-rest-open-reviews-for-project (project)
  (interactive "sEnter gerrit project: ")
  (let* ((json-array-type 'list)
         (req (format (concat "/changes/?q=is:open+project:%s&"
                              "o=DOWNLOAD_COMMANDS&"
                              "o=CURRENT_REVISION&"
                              "o=CURRENT_COMMIT&"
                              "o=DETAILED_LABELS&"
                              "o=DETAILED_ACCOUNTS")
                      (funcall 'gerrit-rest--escape-project project)))
         (resp (gerrit-rest-sync "GET" nil req)))
    ;; (setq open-reviews-response resp) ;; for debugging only (use M-x ielm)
    resp))

(provide 'gerrit-rest)

;;; gerrit-rest.el ends here