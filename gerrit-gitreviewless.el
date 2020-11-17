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

;; for testing
;; (let ((default-directory "/home/thomas.hisch/sandbox/pltb301/jobdeck"))
;;   (gerrit-download-new-v3))
;; "refs/changes/36/35436/1"


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
                           (number-to-string change-nr)))
         (change-owner (alist-get (gerrit--alist-get-recursive
                                   'owner '_account_id change-metadata)
                                  gerrit--accounts-alist))
         ;; TODO change-owner is escaped by git-review (_ instead of . is used)
         ;; git-review uses re.sub(r'\W+', "_", ownername), which was introduced
         ;; more than 8 years ago.
         (local-branch (format "review/%s/%s" change-owner change-topic)))

    ;; TODO log messages?

    ;; this runs async, which is not what I want, because then I can't run
    ;; other git commands afterwards (magit-fetch-refspec
    ;; (gerrit-get-remote) (gerrit--get-refspec change-metadata) nil)

    ;;TODO
    ;; this next call doesn't work if the authorization doesn't work
    ;; (e.g. if ssh-add was not called)
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

(defun gerrit--ensure-commit-msg-hook-exists ()
  (let ((hook-file  (magit-git-dir "hooks/commit-msg")))
    (unless (file-exists-p hook-file)
      (message "downloading commit-msg hook file")
      (url-copy-file
       (concat "https://" gerrit-host  "/tools/hooks/commit-msg") hook-file)
      (set-file-modes hook-file #o755))))

(defun gerrit-push-and-assign (assignee &rest push-args)
  "Execute Git push with PUSH-ARGS and assign changes to ASSIGNEE.

A section in the respective process buffer is created."
  (interactive)
  (progn
    (apply #'magit-run-git-async "push" push-args)
    (set-process-sentinel
     magit-this-process
     (lambda (process event)
       (when (memq (process-status process) '(exit signal))
         (when (buffer-live-p (process-buffer process))
           (with-current-buffer (process-buffer process)
             (when-let ((section (get-text-property (point) 'magit-section))
                        (output (buffer-substring-no-properties
                                 (oref section content)
                                 (oref section end))))
               (if (not (zerop (process-exit-status process)))
                   ;; error
                   (magit-process-sentinel process event)

                 ;; success
                 (process-put process 'inhibit-refresh t)

                 ;; parse the output of "git push" and extract the change numbers. This
                 ;; information is used for setting the specified assignee
                 ;; Alternatively we could perform a gerrit query with owner:me and set the
                 ;; assignee for the latest change(s).
                 (message "output of git push %s" output)
                 (unless (equal "" assignee)
                   (if-let ((matched-changes (s-match-strings-all "/\\+/[0-9]+" output)))
                       (seq-do (lambda (x) (let ((changenr (s-chop-prefix "/+/" (car x))))
                                        (message "Setting assignee of %s to %s" changenr assignee)
                                        ;; TODO create a new section in the magit process buffer
                                        (gerrit-rest--set-assignee changenr assignee)))
                               matched-changes)))
                 (magit-process-sentinel process event))))))))))

(defun gerrit-magit-process-buffer-add-item (msg &rest args)
  "Create a new section and write message MSG into magit process buffer.

MSG needs to be a string and ARGS are the args are used for the
section header."
  (interactive)
  (let (mpf)
    (unwind-protect
        (progn
          (setq mpf (make-temp-file "gerrit-magit-process-file"))
          (delete-file mpf)
          (write-region msg nil mpf)
          (with-current-buffer (magit-process-buffer t)
            (magit-process-insert-section default-directory
                                          "REST"
                                          args 0
                                          mpf
                                          )))
      (ignore-errors (delete-file mpf)))))

(defun gerrit--get-upload-refspec ()
  ;; FIXME magit-get-upstream branch may return nil if
  ;; no upstream configured for branch ...
  (concat "refs/for/" (cadr (s-split "/" (magit-get-upstream-branch)))))

(defun gerrit-upload--new (assignee reviewers topic ready-for-review wip)
  "Push the current changes/commits to the gerrit server and set metadata."

  (gerrit--ensure-commit-msg-hook-exists)
  ;; TODO check that all to-be-uploaded commits have a changeid line
  (message "topic:%s" topic)

  (let ((remote (magit-get-upstream-remote))
        (refspec (gerrit--get-upload-refspec)))
    ;; there are a bunch of push options that are supported by gerrit:
    ;; https://gerrit-review.googlesource.com/Documentation/user-upload.html#push_options
    (let ((push-opts nil))
      (unless (equal "" topic)
        (push (concat "topic=" topic) push-opts))
      (when ready-for-review
        (push "ready" push-opts))
      (when wip
        (push "wip" push-opts))

      (cl-loop for reviewer in reviewers do
               ;; TODO check that reviewers are valid (by checking that all
               ;; reviewers don't contain a white-space)
               (push (concat "r=" reviewer) push-opts))

      (when push-opts
        (setq refspec (concat refspec "%" (s-join "," push-opts)))))
    (message "refspec:%s" refspec)

    (message "assignee:%s" assignee)
    (gerrit-push-and-assign
     assignee
     "--no-follow-tags"
     remote
     (concat "HEAD:" refspec))))

(defun gerrit-upload-run-new ()
  (interactive)
  (gerrit-upload--new
   gerrit-last-assignee
   gerrit-last-reviewers
   gerrit-last-topic
   gerrit-upload-ready-for-review
   nil))

(defhydra hydra-gerrit-upload-new-v1 (:color amaranth ;; foreign-keys warning, blue heads exit hydra
                                             :hint nil ;; show hint in the echo area
                                             :columns 1
                                             :body-pre (progn
                                                         (gerrit-load-lists)
                                                         (setq gerrit-last-topic "")
                                                         (setq gerrit-last-reviewers nil)
                                                         (setq gerrit-last-assignee "")
                                                         (setq gerrit-upload-ready-for-review nil))
                                             :after-exit (gerrit-save-lists))
  "
gerrit-upload-new:
"
  ("r" gerrit-upload-add-reviewer "Add reviewer")
  ("R" gerrit-upload-remove-reviewer "Remove reviewer")
  ("a" gerrit-upload-set-assignee "Set assignee")
  ("t" gerrit-upload-set-topic "Set topic")
  ("v" gerrit-upload-toggle-ready-for-review "Toggle ready-for-review")
  ("G" gerrit-upload-run-new "Upload" :color blue))

(defalias 'gerrit-upload-new #'hydra-gerrit-upload-new-v1/body)

(provide 'gerrit-gitreviewless)
