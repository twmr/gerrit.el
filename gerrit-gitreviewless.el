;;; gerrit-gitreviewless.el --- git-review components implement in elisp -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Thomas Hisch <t.hisch@gmail.com>
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


;; TODO how do I access the value of "boo" here if I have the keyname as a
;; string?
;; (setq my-alist (gerrit-rest--read-json
;;                 "{\"key\": \"boo\", \"main\":{\"foo\": 4, \"boo\": 3}}"))
;; ((key . "boo") (main (foo . 4) (boo . 3)))

;; (gerrit--alist-get-recursive 'main "boo" my-alist) doesn't work

;; (gerrit--alist-get-recursive 'main 'boo my-alist)
;; 3

;; (gerrit--alist-get-recursive 'main
;;                              (intern (alist-get 'key my-alist))
;;                              my-alist)
;; 3

(defun gerrit-format-change (change)
  (concat
   (propertize (number-to-string (alist-get '_number change)) 'face 'magit-hash)
   " "
   (propertize (alist-get 'branch change) 'face 'magit-branch-remote)
   " "
   (propertize (alist-get 'subject change) 'face 'magit-section-highlight)
   ))

(defun gerrit--get-refspec(change-metadata)
  ;; this is important for determining the refspec needed for
  ;; git-fetch
  ;; change-ref is e.g. "refs/changes/16/35216/2"
  (let* ((revisions (alist-get 'revisions change-metadata))
         (revision (alist-get 'current_revision change-metadata)))
    (gerrit--alist-get-recursive (intern revision) 'ref revisions)))

(defun gerrit--download-change (change-metadata)
  (let* ((change-nr (alist-get '_number change-metadata))
         (change-branch (alist-get 'branch change-metadata))
         (change-topic (or (alist-get 'topic change-metadata)
                           (number-as-string change-nr)))
         ;; TODO ensure that gerrit--acounts-alist is initialized
         (change-owner (alist-get (gerrit--alist-get-recursive
                                   'owner '_account_id change-metadata)
                                  gerrit--accounts-alist))
         (local-branch (format "reviews/%s/%s" change-owner change-topic)))

    ;; TODO log messages?

    ;; this runs async, which is not what I want, because then I can't
    ;; run other git commands afterwards
    ;; (magit-fetch-refspec (gerrit-get-remote) (gerrit--get-refspec change-metadata) nil)
    (magit-call-git "fetch" (gerrit-get-remote) (gerrit--get-refspec change-metadata))
    ;; TODO handle errors of magit-branch-and-checkout:
    ;; check if a local branch with this name already exists
    ;; if yes, determine the remote and remote-branch of this local-branch
    ;;         and reuse it only if they are equal with the current-remote
    ;;         (gerrit-get-remote)
    ;;              and `change-branch`
    (magit-branch-and-checkout local-branch "FETCH_HEAD")))


(defun gerrit-download-new-v3 ()
  "Download change from the gerrit server."
  (interactive)
  (gerrit--init-accounts)
  (let* ((open-changes
          (seq-map #'gerrit-format-change (gerrit-rest-change-query
                                           (concat "status:open project:"
                                                   (gerrit-get-current-project)))))
         (selected-line (completing-read
                         "Download Change: " open-changes nil nil))
         (changenr (car (s-split " " (s-trim selected-line))))

         ;; the return value of `gerrit-rest-change-query` contains the
         ;; current revision, but not the one of `gerrit-rest-change-get`.
         (change-metadata (car (gerrit-rest-change-query changenr))))

    (gerrit--download-change change-metadata)))


;; for testing
;; (let ((default-directory "/home/thomas.hisch/sandbox/pltb301/jobdeck"))
;;   (gerrit-download-new-v3))
;; "refs/changes/36/35436/1"

(provide 'gerrit-gitreviewless)
