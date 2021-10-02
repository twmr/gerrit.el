;;; gerrit.el --- Gerrit client -*- lexical-binding: t; -*-

;; Author: Thomas Hisch <t.hisch@gmail.com>
;; Maintainer: Thomas Hisch <t.hisch@gmail.com>
;; URL: https://github.com/thisch/gerrit.el
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (hydra "0.15.0") (magit "2.13.1") (s "1.12.0") (dash "0.2.15"))
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; this program; see the file LICENSE. If not, write to the write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; This package contains
;;
;; * functions for downloading and uploading a change
;; * (`gerrit-upload-transient` and `gerrit-download`)
;;
;; * `gerrit-dashboard`, function for displaying a dashboard, similar to the
;;   one of the gerrit web-interface
;;
;; * open-reviews section for the magit-status buffer (`magit-gerrit-insert-status`)
;;
;;     section local keymap:
;;        RET - opens change in browser
;;
;;  See the README.md on the github project page for more information.

;;; Code:

(require 'cl-lib)  ;; for cl-remove-duplicates
(require 'cl-extra)  ;; for cl-some
(require 'dash)
(require 'hydra)
(require 'magit)
(require 'magit-margin) ;; for magit--age
(require 'magit-section)
(require 'recentf)
(require 's)
(require 'subr-x) ;; for string-empty-p

(require 'gerrit-rest)

(defvar gerrit--accounts-alist nil)

(defvar gerrit-dashboard-buffer-name "*gerrit-dashboard*" nil)
(defvar gerrit-dashboard-query-alist
  '(("Assigned to me" . "assignee:self (-is:wip OR owner:self OR assignee:self) is:open -is:ignored")
    ("Work in progress" . "is:open owner:self is:wip")
    ("Outgoing reviews" . "is:open owner:self -is:wip -is:ignored")
    ("Incoming reviews" .  "is:open -owner:self -is:wip -is:ignored (reviewer:self OR assignee:self)")
    ("CCed On" . "is:open -is:ignored cc:self")
    ("Recently closed" . "is:closed -is:ignored (-is:wip OR owner:self) (owner:self OR reviewer:self OR assignee:self OR cc:self) limit:15"))
  "Query search string that is used for the data shown in the gerrit-dashboard.")

(defgroup gerrit nil
  "Maintain a menu of recently opened files."
  :version "25.1"
  ;; which group should be used?
  :group 'files)

(defcustom gerrit-host nil
  "Hostname of the gerrit instance (without the protocol prefix)."
  :group 'gerrit
  :type 'string)

(defcustom gerrit-change-max-nr-digits 5
  "Number of digits used for displaying gerrit changes."
  :group 'gerrit
  :type 'int)

(defcustom gerrit-use-gitreview-interface t
  "If t, use deprecated git-review interface.

Otherwise, the new REST-only interface of gerrit-upload and
gerrit-download is used."
  :group 'gerrit
  :type 'boolean)

(defun gerrit--init-accounts ()
  "Intialize `gerrit--accounts-alist`."
  (unless gerrit--accounts-alist
    (message "Fetching gerrit accounts ...")
    (setq gerrit--accounts-alist (gerrit-rest--get-gerrit-accounts))
    (message "Fetched gerrit accounts (len=%d)" (length gerrit--accounts-alist))))

(defun gerrit--read-assignee ()
  "Ask for the name of an assignee."
  (completing-read
   "Assignee: "
   (seq-map #'cdr gerrit--accounts-alist) ;; usernames
   nil ;; predicate
   t ;; require match
   nil ;; initial
   nil ;; hist (output only?)
   ;; def
   nil))



;; gerrit-upload* and gerrit-download* functions
;;
;; TODO finalize API (think about a consistent naming scheme (when to use -- and when not)
;; DONE copy the gitreviewless code into gerrit.el file
;; TODO provide a C-u version for gerrit-download, which also asks for the PS number
;; TODO defvar for turning off .gitreview parsing. I would rather not parse it.
;;         use "origin" as the remote if this parsing is turned off
;; TODO if .gitreview parsing is turned off - how do we determine the upstream branch then?
;;         if the local branch has an upstream configured -> use it
;;         if it doesn't -> ask the user (magit-read-string)
;;         Note:
;;         git checkout -b fb -t origin/version0.2  # this can be used for creating
;;         a local branch fb that is based on origin/version0.2 and which tracks
;;         origin/version0.2 (=upstream)
;; TODO write some unit tests that create git repos and test elisp functions
;; DONE use gerrit-magit-process-buffer-add-item for:
;;       setting the assignee:
;;            section name: changenr and assignee
;;            section body: maybe rest output? especially useful if there is an error
;; TODO download gerrit change on top of current branch (like cherry-pick)
;; TODO download command that lists only the changes in the current branch
;; TODO check (in advance) that the uploaded commits contain a Change-Id
;;       see (magit-insert-log "@{upstream}.." args)
;;       for commit in $(git rev-list 15044377058d2481e1d9a1334c71037598f9a006..HEAD); do
;;           git log --format=%B -r $commit -n1; done
;;       or check if 'Change-Id string is in the output of
;;           git log --format=%B -r start-sha1..end-sha1
;;       how do I determine the start-sha1?
;;           -> use  branchname@{u}
;; TODO upload: before uploading a change check the sha1 of the latest commit and check
;;              if it is already a change on gerrit with this sha1
;; TODO upload: always-rebase by default? git-review always rebases by default
;;              but for merge commits it must not rebase automatically
;; DONE use transient for upload form
;; -> The test-transient allows one to cycle over all settings C-M-p / C-M-n
;; and also over all the infix history

;; this variable is used in `gerrit-download-format-change'
(defvar gerrit-change-singleline-columns
  '(number branch subject)
  "List of shown columns for change-selection.

  List of columns that should be displayed in functions that ask
  the user to select a change from a list of changes.

  Currently supported columns are:
  'number (the change number)
  'branch (the branch of the change)
  'subject (the subject of the commit msg)
  'project (the project name)"
  )

(defun gerrit-download-format-change (change)
  (let (columns)
    ;; can this be implemented in an easier way?
    (when (member 'number gerrit-change-singleline-columns)
      (push (propertize (number-to-string
                         (alist-get '_number change)) 'face 'magit-hash) columns))
    (when (member 'project gerrit-change-singleline-columns)
      (push (propertize (alist-get 'project change) 'face 'magit-branch-remote) columns))
    (when (member 'branch gerrit-change-singleline-columns)
      (push (propertize (alist-get 'branch change) 'face 'magit-branch-remote) columns))
    (when (member 'subject gerrit-change-singleline-columns)
      (push (propertize (alist-get 'subject change) 'face 'magit-section-highlight) columns))
    (s-join " " (nreverse columns))))

(defun gerrit-download--get-refspec (change-metadata)
  "Return the refspec of a gerrit change from CHANGE-METADATA.

This refspec is a string of the form 'refs/changes/xx/xx/x'.
"
  ;; this is important for determining the refspec needed for
  ;; git-fetch
  ;; change-ref is e.g. "refs/changes/16/35216/2"
  (let* ((revisions (alist-get 'revisions change-metadata))
         (revision (alist-get 'current_revision change-metadata)))
    (gerrit--alist-get-recursive (intern revision) 'ref revisions)))

(defun gerrit--get-tracked (branch)
  "Get upstream-remote and upstream-branch of a local BRANCH."
  ;; Note that magit-get-upstream-branch returns a propertized string
  (let ((tracked (magit-get-upstream-branch branch)))
    (s-split-up-to "/" tracked 1 t)))


(defun gerrit--download-change (change-metadata)
  ;; to see what git-review does under the hood - see:
  ;; strace -z -f -e execve git-review -d 3591
  (let* ((change-nr (alist-get '_number change-metadata))
         (change-branch (alist-get 'branch change-metadata))
         (change-topic (or (alist-get 'topic change-metadata)
                           (number-to-string change-nr)))
         (change-owner (alist-get (gerrit--alist-get-recursive
                                   'owner '_account_id change-metadata)
                                  gerrit--accounts-alist))
         (local-branch (format "review/%s/%s"
                               ;; change-owner is 'escaped' by git-review (_
                               ;; instead of . is used). git-review uses
                               ;; re.sub(r'\W+', "_", ownername), which was
                               ;; introduced 2011 (commit 08bd9c). I don't
                               ;; know why they did it.
                               (replace-regexp-in-string "\\W+" "_" change-owner)
                               change-topic)))

    ;; This next (async) call ensures that the authorization works
    ;; (e.g. if ssh-add was not called) this async call runs
    ;; magit-process-password-prompt-regexps (used in magit-process-filter)
    ;; which is called in magit-start-process
    (magit-run-git-async "fetch"
                         (gerrit-get-remote)
                         (gerrit-download--get-refspec change-metadata))
    (set-process-sentinel
     magit-this-process
     (lambda (process event)
       (when (memq (process-status process) '(exit signal))
         (if (not (zerop (process-exit-status process)))
             ;; error
             (magit-process-sentinel process event)

           ;; git-fetch was successful

           ;; see magit-delete-remote-branch-sentinel
           (process-put process 'inhibit-refresh t)
           (magit-process-sentinel process event)

           (if-let* ((local-ref (concat "refs/heads/" local-branch))
                     (branch-exists (magit-git-success
                                     "show-ref" "--verify" "--quiet" local-ref)))
               (progn
                 ;; since local-branch exists, gerrit--get-tracked never returns nil
                 (seq-let (tracked-remote tracked-branch) (gerrit--get-tracked local-branch)
                   (unless (and (equal tracked-remote (gerrit-get-remote))
                                (equal tracked-branch change-branch))
                     (magit-refresh)
                     (error "Branch tracking incompatibility: Tracking %s/%s instead of %s/%s"
                            tracked-remote tracked-branch
                            (gerrit-get-remote) change-branch)))
                 (magit-call-git "checkout" local-branch)
                 (magit-call-git "reset" "--hard" "FETCH_HEAD"))

             (magit-call-git "checkout" "-b" local-branch "FETCH_HEAD")
             ;; set upstream here (see checkout_review function in cmd.py)
             ;; this upstream branch is needed for rebasing
             (magit-call-git "branch"
                             "--set-upstream-to"
                             (format "%s/%s" (gerrit-get-remote) change-branch)
                             local-branch))
           (magit-refresh)))))))

(defun gerrit-download--new (changenr)
  "Download change CHANGENR from the gerrit server using REST interface."
  (let ((change-metadata (car (gerrit-rest-change-query changenr))))
    ;; the return value of `gerrit-rest-change-query` contains the
    ;; current revision, but not the one of `gerrit-rest-change-get`.
    (gerrit--download-change change-metadata)))

(defun gerrit--ensure-commit-msg-hook-exists ()
  "Create a commit-msg hook, if it doesn't exist."
  (let ((hook-file (magit-git-dir "hooks/commit-msg")))
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
                 (when assignee
                   (if-let ((matched-changes (s-match-strings-all "/\\+/[0-9]+" output)))
                       (seq-do (lambda (x) (let ((changenr (s-chop-prefix "/+/" (car x))))
                                        (message "Setting assignee of %s to %s" changenr assignee)
                                        (gerrit-rest-change-set-assignee changenr assignee)
                                        (gerrit-magit-process-buffer-add-item
                                         (format "Assignee of change %s was set to %s" changenr assignee)
                                         "set-assignee" changenr)))
                               matched-changes)))
                 (magit-process-sentinel process event))))))))))

(defun gerrit-upload--get-refspec ()
  (concat "refs/for/" (gerrit-get-upstream-branch)))

;; The transient history is saved when the kill-emacs-hook is run, which is
;; run when (kill-emacs) is called. Make sure that you run kill-emacs when
;; you stop emacs (or restart an emacs (systemd) service).  Note that
;; (transient-save-history) is the function that saves the history.

;; There is a limit for the number of entries saved per option(?) into the
;; history file, which is 10 by default. I think it makes sense to increase
;; this value to at least 50 (only 10 saved topic names may not be enough).

;; The `history` in the reader callbacks is updated after the reader
;; callback was called.

;; The history file contains both the history elements of "submitted"
;; settings (where the action was called) as well as the history of the
;; individual options independent whether the action was called or not (if a
;; reader is specified, the history parameter needs to be updated for this
;; to work!).

(defun gerrit-upload:--action (&optional args)
  "Push the current changes/commits to the gerrit server and set metadata."
  (interactive
   (list (transient-args 'gerrit-upload-transient)))

  (gerrit--ensure-commit-msg-hook-exists)
  ;; TODO check that all to-be-uploaded commits have a changeid line

  (let (assignee
        push-opts
        (remote (gerrit-get-remote))
        (refspec (gerrit-upload--get-refspec)))
    ;; there are a bunch of push options that are supported by gerrit:
    ;; https://gerrit-review.googlesource.com/Documentation/user-upload.html#push_options

    ;; I don't like this handling of transient-args, maybe transient can
    ;; pass alists to gerrit-upload--action instead of a list of strings
    (cl-loop for arg in args do
             (cond ((s-starts-with? "reviewers=" arg)
                    (cl-loop for reviewer in (s-split "," (s-chop-prefix "reviewers=" arg)) do
                             ;; TODO check that reviewers are valid (by checking that all
                             ;; reviewers don't contain a white-space)
                             (push (concat "r=" reviewer) push-opts)))
                   ((s-starts-with? "assignee=" arg)
                    (setq assignee (s-chop-prefix "assignee=" arg)))
                   ((s-starts-with? "topic=" arg)
                    (push  arg push-opts))
                   ((string= "ready" arg)
                    (push "ready" push-opts))
                   ((string= "wip" arg)
                    (push "wip" push-opts))
                   (t
                    (error (format "no match for arg: %s" arg)))))

    (when push-opts
      (setq refspec (concat refspec "%" (s-join "," push-opts))))

    (gerrit-push-and-assign
     assignee
     "--no-follow-tags" ;; don't error when encountering local tags, which
                        ;; are absent from gerrit.
     remote
     (concat "HEAD:"  refspec))))

(transient-define-prefix gerrit-upload-transient ()
  "Transient used for uploading changes to gerrit"
  ["Arguments"
   (gerrit-upload:--reviewers)
   (gerrit-upload:--assignee)
   ("w" "Work in Progress" "wip")
   ("v" "Ready for Review" "ready")
   (gerrit-upload:--topic)
  ]
  ["Actions"
   ("u" "Upload" gerrit-upload:--action)])

;; TODO ask on github why a subclass of transient option is needed.
(defclass gerrit-multivalue-option (transient-option) ())

(cl-defmethod transient-infix-value ((obj gerrit-multivalue-option))
  "Return (concat ARGUMENT VALUE) or nil.

ARGUMENT and VALUE are the values of the respective slots of OBJ.
If VALUE is nil, then return nil.  VALUE may be the empty string,
which is not the same as nil."
  (when-let ((value (oref obj value)))
    (if (listp value) (setq value (string-join value ",")))
    (concat (oref obj argument) value)))

(transient-define-argument gerrit-upload:--reviewers ()
  :description "Reviewers"
  :class 'gerrit-multivalue-option
  :key "r"
  :argument "reviewers="
  ;; :format " %k %v"
  :multi-value t
  :reader 'gerrit-upload:--read-reviewers)

(defun gerrit-upload:--read-reviewers (prompt _initial-input _history)
  (gerrit--init-accounts)
  ;; FIXME the sorting order here seems to be different than the one used in
  ;; completing-read! Maybe this is just an ivy issue
  (completing-read-multiple
   prompt
   (seq-map #'cdr gerrit--accounts-alist) ;; usernames
   nil
   nil
   nil))

(transient-define-argument gerrit-upload:--assignee ()
  :description "Assignee"
  :class 'transient-option
  :key "a"
  :argument "assignee="
  :reader 'gerrit-upload:--read-assignee)

(transient-define-argument gerrit-upload:--topic ()
  :description "Topic"
  :class 'transient-option
  :key "t"
  :argument "topic="
  :reader 'gerrit-upload:--read-topic)

(defun gerrit-upload:--read-assignee (prompt _initial-input history)
  (gerrit--init-accounts)
  ;; (gerrit--read-assignee) this doesn't update the history

  ;; using the history here doesn't have an effect (maybe it does, but for
  ;; ivy-completing-read it doesn't)
  (completing-read
   prompt
   (seq-map #'cdr gerrit--accounts-alist) ;; usernames
   nil ;; predicate
   t ;; require match
   nil ;; initial ;; Maybe it makes sense to use the last/first history element here
   history ;; hist (output only?)
   ;; def
   nil))

(defun gerrit-upload:--read-topic (prompt _initial-input history)
  (completing-read
   prompt
   (symbol-value history)
   nil nil nil
   history))



(defun gerrit-magit-process-buffer-add-item (msg &rest args)
  "Create a new section and write message MSG into magit process buffer.

MSG needs to be a string and ARGS are the args are used for the
section header."
  (with-temp-buffer
    (insert msg)
    (let ((msg-buffer (get-buffer (buffer-name))))
    (with-current-buffer (magit-process-buffer t)
      (magit-process-insert-section default-directory
                                    "REST"
                                    args nil
                                    msg-buffer)))))


(defun gerrit-magit-insert-status ()
  "Show all open gerrit reviews when called in the magit-status-section via `magit-status-section-hook'."

 (when-let ((fetched-reviews (condition-case nil
                                 (gerrit-rest-open-reviews-for-project (gerrit-get-current-project))
                               (error '()))))
   (magit-insert-section (open-reviews)
     (magit-insert-heading "Open Gerrit Reviews")
     (let* ((fetched-reviews-string-lists
             (seq-map (lambda (change) (list
                                   (number-to-string (cdr (assoc '_number change)))
                                   (cdr (assoc 'branch change))
                                   (or (cdr (assoc 'topic change)) "") ;; topic may be nil
                                   (cdr (assoc 'subject change))))
                      (seq-map #'cdr fetched-reviews)))
            (max-column-sizes (seq-reduce
                               (lambda (a b) (--zip-with (max it other)
                                                    a ;; list of ints
                                                    (seq-map #'length b) ;; convert list of strs to list of numbers
                                                    ))
                               ;; results is a list of lists of strings
                               fetched-reviews-string-lists
                               ;; initial value
                               (mapcar #'length (car fetched-reviews-string-lists))))

            ;; TODO only left-align topic and subject?
            (format-str (mapconcat (lambda (x) (concat "%-" (number-to-string x) "s")) max-column-sizes " ")))

       (seq-do (lambda (review)
                 (seq-let (number topic branch subject) review
                   (magit-insert-section (open-reviews-issue review t)
                     (magit-insert-heading
                       (format format-str
                               (propertize number 'face 'magit-hash)
                               (propertize topic 'face 'magit-tag)
                               (propertize branch 'face 'magit-branch-remote)
                               (propertize subject 'face 'magit-section-highlight))))))
               fetched-reviews-string-lists))
     (insert ?\n))))

;; don't rename this var, as it is required for magit-sections
(defvar magit-open-reviews-issue-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'gerrit-magit-open-reviews--open-gerrit-change)
    map)
  "Keymap for `magit-open-reviews' top level section.

The prefix magit- prefix is required by `magit-insert-section'.")

(defun gerrit-magit-open-reviews--open-gerrit-change ()
  "Open the gerrit change under point in the browser."
  (interactive)
  (browse-url (format
               "https://%s/c/%s"
               gerrit-host
               ;; (oref (magit-current-section) value) returns the object
               ;; passed as the 2nd arg to (magit-insert-section)

               ;; TODO change code s.t. s-chop-prefix is not needed
               (s-chop-prefix "#"
                              (nth 0 (oref (magit-current-section) value))))))

(defun gerrit-get-remote ()
  "Return the name of the remote."
  ;; TODO read the data from a cache
  (or (magit-git-string "config" "-f" (expand-file-name ".gitreview" (magit-toplevel))
                       "--get" "gerrit.defaultremote")
      "origin"))

(defun gerrit-get-upstream-branch ()
  "Return the name of the upstream branch.

The returned string is not prefixed with the remote."
  ;; TODO read the data from a cache
  (or (when-let ((upstream-branch (magit-get-upstream-branch)))
        (cadr (s-split "/" upstream-branch)))
      (magit-git-string "config" "-f" (expand-file-name ".gitreview" (magit-toplevel))
                        "--get" "gerrit.defaultbranch")
      (when-let ((upstream-branch (magit-read-upstream-branch
                                   nil
                                   (concat "No upstream branch is configured, please specify one "
                                           "(starting with the remote)"))))
        (magit-set-upstream-branch (magit-get-current-branch) upstream-branch)
        (cadr (s-split "/" upstream-branch)))))

(defun gerrit-get-current-project ()
  "Return the gerrit project name, e.g., 'software/jobdeck'."
  (interactive)
  (let* ((remote-url (car
                      (magit-config-get-from-cached-list
                       (format "remote.%s.url" (gerrit-get-remote)))))
         (parsed-url (url-generic-parse-url remote-url))
         (parsed-path (url-filename parsed-url)))
    (cond ((not (string= remote-url parsed-path)) ; Any URL with scheme:// AND scp-like w/o username
           (replace-regexp-in-string "^/?\\(.*?\\)\\(\\.git/?\\)?$" "\\1"
                                     parsed-path))
          ((string-match "^\\(?:[^@]+@\\)?[^:]+:\\(.*?\\)\\(?:\\.git/?\\)?$" remote-url) ; scp-like
           ;; the matching regex is very generous, assume the URL is correct
           (match-string 1 remote-url))
          (t (error "Remote URL %s is not recognized" remote-url)))))

(defun gerrit-get-changeid-from-current-commit ()
  "Determine the change-id from the current commit.

A string like the following is returned:
I8473b95934b5732ac55d26311a706c9c2bde9940"
  (let* (
        (commit-message-lines (magit-git-lines "log" "-1" "--pretty=%B"))
        (change-id-line
         ;; in the case of cherry-picks, the Change-Id line may not be the
         ;; last line of the commit message. Therefore, iterate over all
         ;; lines of the commit until a match is found.
         (cl-some (lambda (line) (and (s-starts-with? "Change-Id: " line) line))
                  commit-message-lines)))

    (unless change-id-line
      (error "Commit message doesn't end with a change-id"))
    (s-chop-prefix "Change-Id: " change-id-line)))

(defun gerrit-get-unique-changeid-from-current-commit ()
  "Determine the unique change-id from the current commit.

A string like the following is returned:
myProject~master~I8473b95934b5732ac55d26311a706c9c2bde9940"
  (let ((branch (substring-no-properties (gerrit-get-upstream-branch)))
        (project (substring-no-properties (gerrit-get-current-project))))
    (concat (gerrit-rest--escape-project project)
            "~"
            branch
            "~"
            (gerrit-get-changeid-from-current-commit))))



;; section (code that uses magit-section.el)

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
;;                comment-author: first-line of comment   date
;;           [changenr] project: commit subject:
;;             commit msg body
;;             current patch:
;;                file C:
;;                file D:
;;             comments:
;;                comment-author: first-line of comment   date

(defun gerrit-section-filter (_message-info)
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
                             max-date-width))))
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
                                                   'magit-log-author)
                                       (s-replace  "Patch Set " "PS" firstline)
                                       (propertize date 'font-lock-face 'magit-log-date))))

                 (magit-insert-section (gerrit-comments)
                   (save-match-data
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
                                          heading))))

                   (magit-insert-heading heading)

                   ;; only show the rest of the message if the message is multi-line
                   (unless (s-blank? restlines)
                     (insert (concat restlines "\n")))))))))

(defun gerrit-section--info (topicname changenr)
  (with-current-buffer (get-buffer-create "*gerrit-section*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (magit-section-mode)
      ;; without this top-level section, I can't toggle the sections for some reason.
      (magit-insert-section (toplevel)
        (when changenr
          (gerrit-section--insert-change-comments (gerrit-rest-get-change-info changenr)))
        (when topicname
          (insert "Topic: " topicname "\n")
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



;; dashboard

(defvar gerrit-dashboard-columns
  [("Number" 8)
   ("Subject" 55)
   ("Status" 10)
   ("Owner" 15)
   ("Assignee" 15)
   ("Repo" 24)
   ("Branch" 12)
   ("Topic" 15)
   ("Updated" 8)
   ("SZ" 3)
   ("CR" 2)
   ("V" 2)]
  "Column-names and column-sizes of the gerrit dashboard.")

(defface gerrit-fail
  `((t (:foreground ,(face-foreground 'magit-diff-removed-highlight))))
  "Used for negative votes."
  :group 'faces)

(defface gerrit-success
  `((t (:foreground ,(face-foreground 'magit-diff-added-highlight))))
  "Used for positive votes."
  :group 'faces)

(defface gerrit-section
  '((t (:inherit 'magit-section-heading)))
  "Used for the section names in the dashboard."
  :group 'faces)

(defun gerrit--combined-level-to-numberstr (combined-label verify)
  (or (if verify
          (pcase combined-label
            ('approved (propertize "+1" 'face 'gerrit-success))
            ('rejected (propertize "❌" 'face 'gerrit-fail)))
        (pcase combined-label
          ('approve (propertize "✔" 'face 'gerrit-success))
          ('recommended (propertize "+1" 'face 'gerrit-success))
          ('disliked (propertize "-1" 'face 'gerrit-fail))
          ('rejected (propertize "-2" 'face 'gerrit-fail))))
      ""))

(defun gerrit--alist-get-recursive (&rest args)
  "Recursively find keys in an alist (last elem of ARGS)."
  (let* ((alist (car (last args)))
         (keys (nbutlast args)))
    (condition-case nil
        (progn
          (while keys
            (setq alist (alist-get (pop keys) alist)))
          alist)
      (error nil))))

(defun gerrit-dashboard--get-change-metadata (change)
  "Convert a json object returned by the `gerrit-rest-change-query` for CHANGE into an alist."
  `((number . ,(alist-get '_number change)) ;; int
    (subject . ,(alist-get 'subject change)) ;; string
    (status . ,(alist-get 'status change)) ;; string
    ;; alist-get 'owner => (_account_id . 1017133)
    (owner . ,(gerrit--alist-get-recursive 'owner '_account_id change))
    (assignee . ,(cdr (car (alist-get 'assignee change)))) ;; optional string
    (repo . ,(alist-get 'project change)) ;; string
    (branch . ,(alist-get 'branch change)) ;; string
    (topic . ,(alist-get 'topic change)) ;; optional string
    (updated . ,(alist-get 'updated change)) ;; string
    (mergeable . ,(alist-get 'mergeable change)) ;; json-boolean
    (insertions . ,(alist-get 'insertions change)) ;; int
    (deletions . ,(alist-get 'deletions change)) ;; int
    (wip . ,(alist-get 'work_in_progress change)) ;; t/nil

    ;; is one of the symbols
    ;; 'rejected, 'approved, 'disliked or 'recommended (or nil)
    (CR-vote . ,(caar (gerrit--alist-get-recursive
                       'labels 'Code-Review change)))
    ;; is one of the symbols
    ;; 'approved or 'disliked (or nil)
    (verified . ,(caar (gerrit--alist-get-recursive
                        'labels 'Verified change)))))

(defun gerrit--format-abbrev-date (datestr)
  "Convert DATESTR to pretty relative time (eg. 3min ago)."
  ;; take a look at magit-log-format-author-margin (style = age-abbreviated)
  (let ((abbr t))
    (apply #'format (if abbr "%2i%c ago" "%s %s ago")
           (magit--age
            (float-time
             (apply #'encode-time
                    (parse-time-string
                     datestr)))
            abbr))))

(defun gerrit-dashboard--change-metadata-2-entry (change-metadata)
  `[,(propertize
      (number-to-string (alist-get 'number change-metadata))
      'face 'magit-hash)
    ,(propertize (alist-get 'subject change-metadata) 'face 'magit-section-highlight)

    ,(if (eq (alist-get 'mergeable change-metadata) :json-false)
         (propertize "Merge conflict" 'face 'gerrit-fail)
       (if (alist-get 'wip change-metadata)
           "WIP"
         (let ((status (alist-get 'status change-metadata)))
           (pcase status
             ("NEW" "-")
             ("MERGED" (propertize "Merged" 'face 'gerrit-success))
             ("ABANDONED" "Abandoned")
             (_ "")))))

    ,(propertize (or (alist-get (alist-get 'owner change-metadata) gerrit--accounts-alist) "")
                 'face 'magit-log-author)
    ,(propertize (or (alist-get (alist-get 'assignee change-metadata) gerrit--accounts-alist) "")
                 'face 'magit-log-author)
    ,(alist-get 'repo change-metadata)
    ,(propertize (alist-get 'branch change-metadata)
                 'face 'magit-branch-remote)
    ,(propertize (or (alist-get 'topic change-metadata) "")
                 'face 'magit-tag)

    ,(gerrit--format-abbrev-date (alist-get 'updated change-metadata))

    ;; TODO finish this
    ,(if (< (+ (alist-get 'deletions change-metadata)
               (alist-get 'insertions change-metadata)) 15) "S" "L")
    ,(gerrit--combined-level-to-numberstr (alist-get 'CR-vote change-metadata) nil)
    ,(gerrit--combined-level-to-numberstr (alist-get 'verified change-metadata) t)
    ])

(defun gerrit-dashboard--get-data (expression)
  "Return a list with \"tabulated-list-entries\" matching a gerrit search query EXPRESSION."
  (gerrit--init-accounts)
  (seq-map (lambda (change)
             `(nil ,(gerrit-dashboard--change-metadata-2-entry
                    (gerrit-dashboard--get-change-metadata change))))
           (gerrit-rest-change-query expression)))

(defun gerrit-dashboard--entry-number ()
  "Return the change number as a string of a change under point."
  (interactive)
  (aref (tabulated-list-get-entry) 0))

(defun gerrit-dashboard--topic ()
  "Return the topicname as a string of a change under point."
  (interactive)
  (let ((topic-name (aref (tabulated-list-get-entry) 7)))
    (when (string-empty-p topic-name)
      (error "No topic set for the current change"))
    topic-name))

(defun gerrit-dashboard-browse-change ()
  "Open the change under point in a browser."
  (interactive)
  (browse-url (format
               "https://%s/c/%s"
               gerrit-host
               (gerrit-dashboard--entry-number))))

(defun gerrit-dashboard-open-change ()
  "Open the patch of the change under point in a new buffer."
  (interactive)
  (gerrit-rest-change-patch (gerrit-dashboard--entry-number)))

(defun gerrit-dashboard-open-topic ()
  "Open the topic information of a topic under point in a new buffer."
  (interactive)
  (gerrit-section-topic-info (gerrit-dashboard--topic)))

(defun gerrit-dashboard-assign-change ()
  "Set assignee of the change under point."
  (interactive)
  (let ((change-number (gerrit-dashboard--entry-number))
        (assignee (gerrit--read-assignee)))
    (message "setting assignee of change %s to %s" change-number assignee)
    (gerrit-rest-change-set-assignee change-number assignee)
    ;; refresh dashboard
    (gerrit-dashboard--refresh--and-point-restore)))

(defun gerrit-dashboard-assign-change-to-me ()
  "Set assignee of the change under point."
   (interactive)
  (gerrit-rest-change-set-assignee (gerrit-dashboard--entry-number) "self")
  ;; refresh dashboard
  (gerrit-dashboard--refresh--and-point-restore))

(defun gerrit-dashboard--get-list-entries ()
  "Get the all entries used for \"tabulated-list-entries\"."
  (seq-reduce (lambda (acc conscell)
                (let ((section-data
                       (gerrit-dashboard--get-data (cdr conscell))))
                  (append acc `((nil [""
                                      ,(propertize
                                        (format "%s (%d)" (car conscell) (length section-data))
                                        'face 'gerrit-section)
                                      ;; is there an easier way to add len(columns)-2 times ""?
                                      ,@(seq-map (lambda (_) "") (number-sequence 2 (1- (length gerrit-dashboard-columns))))]))
                          section-data)))
              gerrit-dashboard-query-alist '()))

(defun gerrit-dashboard--refresh ()
  "Refresh dashboard."
  (interactive)
  (setq tabulated-list-format gerrit-dashboard-columns)
  (setq tabulated-list-entries (gerrit-dashboard--get-list-entries))
  (tabulated-list-init-header)
  (tabulated-list-print))

(defun gerrit-dashboard--refresh--and-point-restore ()
  "Refresh dashboard and restore current position of point."
  (interactive)
  (let ((ppos (point)))
    (gerrit-dashboard--refresh)
    (goto-char ppos)))

(defvar gerrit-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    ;; TODO vote, ....
    (define-key map (kbd "a") 'gerrit-dashboard-assign-change)
    (define-key map (kbd "A") 'gerrit-dashboard-assign-change-to-me)
    (define-key map (kbd "g") 'gerrit-dashboard--refresh--and-point-restore)
    (define-key map (kbd "o") 'gerrit-dashboard-browse-change)
    (define-key map (kbd "RET") 'gerrit-dashboard-open-change)
    (define-key map (kbd "t") 'gerrit-dashboard-open-topic)
    ;; <C-down> -> forward-paragraph
   map))

(define-derived-mode gerrit-dashboard-mode tabulated-list-mode "gerrit-dashboard"
  "gerrit-dashboard mode"
  (use-local-map gerrit-dashboard-mode-map)

  ;; all lines that don't start with a changenr are header-lines that are
  ;; treated as the beginning of a paragraph
  (setq-local paragraph-start "^[^0-9]")

  ;; some variables have to be made buffer-local s.t. refreshing of
  ;; dashboards works as expected.
  (setq-local gerrit-dashboard-columns gerrit-dashboard-columns)
  (setq-local gerrit-dashboard-query-alist gerrit-dashboard-query-alist)

  (gerrit-dashboard--refresh))

;;;###autoload
(defun gerrit-dashboard ()
  "Show a dashboard in a new buffer."
  (interactive)
  (switch-to-buffer gerrit-dashboard-buffer-name)
  (gerrit-dashboard-mode))



;; legacy functions

;; these two vars are mainly needed for the hydra-based implementation because
;; I don't know how I can communicate between different heads of the hydra
(defvar gerrit-last-reviewers nil)
(defvar gerrit-last-topic nil)
(defvar gerrit-last-assignee nil)
(defvar gerrit-upload-args nil)
(defvar gerrit-upload-ready-for-review nil)

(defvar gerrit-upload-topic-history nil "List of recently used topic names.")
(defvar gerrit-upload-args-history nil "List of recently used args for git-review cmd.")

(defcustom gerrit-upload-max-saved-items 200
  "Maximum number of items of the gerrit lists that will be saved.
A nil value means to save the whole lists."
  :group 'gerrit
  :type 'integer)

(defcustom gerrit-save-file (locate-user-emacs-file ".git-review")
  "File to save the recent lists into."
  ;; Persistency:
  ;; The save/load logic was copied from recentf.el
  ;; Other places in the emacs git repo, where settings are saved/loaded to/from disk are:
  ;;   savehist-mode
  ;;   ...
  ;; See http://mbork.pl/2018-09-10_Persisting_Emacs_variables
  ;; See https://lists.gnu.org/archive/html/help-gnu-emacs/2018-03/msg00120.html

  ;; TODO outsource this persistency code
  :group 'gerrit
  :type 'file)

(defcustom gerrit-upload-default-args ""
  "Default args used when calling 'git review' to upload a change."
  :group 'gerrit
  :type 'string)

(defalias 'gerrit-dump-variable #'recentf-dump-variable)

(defun gerrit-save-lists ()
  "Save the recent lists.
Write data into the file specified by `gerrit-save-file'."
  (interactive)
  (condition-case error
      (with-temp-buffer
        (erase-buffer)
        (set-buffer-file-coding-system 'utf-8-emacs)
        (insert (format-message ";;; Automatically generated on %s.\n"
                                (current-time-string)))
        (gerrit-dump-variable 'gerrit-upload-topic-history gerrit-upload-max-saved-items)
        (insert "\n\n;; Local Variables:\n"
                ";; coding: utf-8-emacs\n"
                ";; End:\n")
        (let ((inhibit-message t))
          (write-file (expand-file-name gerrit-save-file)))
        (set-file-modes gerrit-save-file #o600)
        nil)
    (error
     (warn "gerrit: %s" (error-message-string error)))))

(defun gerrit-load-lists ()
  "Load a previously saved recent list.
Read data from the file specified by `gerrit-save-file'."
  (interactive)
  (let ((file (expand-file-name gerrit-save-file))
        ;; We do not want Tramp asking for passwords.
        (non-essential t))
    (when (file-readable-p file)
      (load-file file))))

(defmacro gerrit-upload-completing-set (msg history)
  "Call `completing-read' using prompt MSG and use the collection HISTORY."
  `(let ((value (completing-read
                 ,msg
                 ,history
                 nil nil nil nil
                 (car ,history))))
     (unless (equal "" value)
       ;; todo simplify the duplicate handling
       (push value ,history)
       (setq ,history (cl-remove-duplicates ,history :test 'string=)))
     value))

(defmacro gerrit-upload-completing-set-with-fixed-collection
    (msg collection history &optional history-excludes)
  "Call `completing-read' using prompt MSG and use the collection COLLECTION.

Contrary to `gerrit-upload-completing-set' this macro uses
a (fixed) collection that may be different from the history
HISTORY of selected values.

To determine the default value in `completing-read' an optional
list HISTORY-EXCLUDES may be used, whose entries are removed from
HISTORY."
  `(let* ((reduced-history (-difference ,history ,history-excludes))
          (value (completing-read
                  ,msg
                  ,collection
                  nil ;; predicate
                  t ;; require match
                  nil ;; initial input
                  nil ;; history
                  ;; default value set to LRU value
                  (car reduced-history))))
     (unless (equal "" value)
       ;; TODO simplify the duplicate handling
       (push value ,history) ;; note that we don't need this if the builtin
                             ;; completing-read is used. Bug in
                             ;; ivy-completing-read?
       (setq ,history (cl-remove-duplicates ,history :test 'string=)))
     value))

(defun gerrit-upload-add-reviewer ()
  "Interactively ask for to-be-added reviewer name."
  (interactive)
  (gerrit--init-accounts)

  ;; exclude the ones from the history that have already been added
  (gerrit-upload-completing-set-with-fixed-collection
         "Reviewer: "
         (seq-map #'cdr gerrit--accounts-alist) ;; usernames
         gerrit-last-reviewers))

(defun gerrit-upload-remove-reviewer ()
  "Interactively ask for to-be-removed reviewer name."
  (interactive)
  (setq gerrit-last-reviewers
        (delete (gerrit-upload-completing-set
                 "Reviewer: "
                 gerrit-last-reviewers)
                gerrit-last-reviewers)))

(defun gerrit-upload-set-assignee ()
  "Interactively ask for an assignee."
  (interactive)
  (gerrit--init-accounts)
  (setq gerrit-last-assignee (gerrit--read-assignee)))

(defun gerrit-upload-set-topic ()
  "Interactively ask for a topic name."
  (interactive)
  (setq gerrit-last-topic (gerrit-upload-completing-set
                           "Topic: "
                           gerrit-upload-topic-history)))

(defun gerrit-upload-set-args ()
  "Interactively ask for arguments that are passed to git-review."
  (interactive)
  (setq gerrit-upload-args (gerrit-upload-completing-set
                            "Args (space separated): "
                            gerrit-upload-args-history)))

(defun gerrit-upload-toggle-ready-for-review ()
  "Toggle git-review's -W parameter on/off."
  (interactive)
  (setq gerrit-upload-ready-for-review (not gerrit-upload-ready-for-review)))

(defun gerrit-upload-create-git-review-cmd ()
  "Create cmdstr for git-review."
  (interactive)
  (let ((reviewers (s-join " " gerrit-last-reviewers)) ;;(sort gerrit-last-reviewers #'string<)))
        (topic gerrit-last-topic)
        (args gerrit-upload-args)
        (cmdstr "git review --yes"))
    (unless (equal "" topic)
      (setq cmdstr (concat cmdstr " -t " topic)))
    (unless (equal "" reviewers)
      (setq cmdstr (concat cmdstr " --reviewers " reviewers)))
    (unless (equal "" args)
      (setq cmdstr (concat cmdstr " " args)))
    (when gerrit-upload-ready-for-review
      (setq cmdstr (concat cmdstr " -W ")))
    cmdstr))

(defun gerrit-upload-run ()
  "Run git-review."
  (interactive)
  (let ((cmdstr (gerrit-upload-create-git-review-cmd)))
    (if (string-empty-p gerrit-last-assignee)
        (magit-git-command cmdstr)
        ;; see #2 (Is it possible to use magit-git-command and pass the
        ;; output of the git review to a defun that sets the assignee?)
        (progn
          ;; TODO create a temporary buffer for the output of git-review?
          (message "Running %s" cmdstr)
          (let ((git-review-output (shell-command-to-string cmdstr)))
            (message "%s" git-review-output)
            (if-let ((matched-changes (s-match-strings-all "/\\+/[0-9]+"
                                                           git-review-output)))
                ;; TODO confirmation?
                (seq-do (lambda (x) (let ((changenr (s-chop-prefix "/+/" (car x))))
                                 (message "Setting assignee of %s to %s" changenr gerrit-last-assignee)
                                 (gerrit-rest-change-set-assignee changenr gerrit-last-assignee)))
                        matched-changes)))))))


(defhydra hydra-gerrit-upload (:color amaranth ;; foreign-keys warning, blue heads exit hydra
                               :hint nil ;; show hint in the echo area
                               :columns 1
                               :body-pre (progn
                                           (gerrit-load-lists)
                                           (setq gerrit-last-topic "")
                                           (setq gerrit-last-reviewers '())
                                           (setq gerrit-last-assignee "")
                                           (setq gerrit-upload-args gerrit-upload-default-args)
                                           (setq gerrit-upload-ready-for-review nil))
                               :after-exit (gerrit-save-lists))
  "
gerrit-upload: (current cmd: %(concat (gerrit-upload-create-git-review-cmd)))
"
  ("r" gerrit-upload-add-reviewer "Add reviewer")
  ("R" gerrit-upload-remove-reviewer "Remove reviewer")
  ("a" gerrit-upload-set-assignee "Set assignee")
  ("t" gerrit-upload-set-topic "Set topic")
  ("v" gerrit-upload-toggle-ready-for-review "Toggle ready-for-review")
  ("A" gerrit-upload-set-args "Set additional args")
  ("RET" gerrit-upload-run "Upload" :color blue))

(defun gerrit-download--gitreview (changenr)
  "Download change with CHANGENR from the gerrit server using git-review."
  (magit-git-command (format "git review -d %s" changenr)))



(defun gerrit--select-change-from-matching-changes (search-string)
  ;; see https://gerrit-review.googlesource.com/Documentation/user-search.html
  (let* ((open-changes (seq-map #'gerrit-download-format-change
                                (gerrit-rest-change-query
                                 (or search-string "is:open")
                                 )))
         (selected-line (completing-read
                         "Download Change: " open-changes nil nil))
         (changenr (car (s-split " " (s-trim selected-line)))))
    changenr))

(transient-define-argument gerrit-download:--branch ()
  :description "Branch"
  :class 'transient-option
  :key "b"
  :argument "branch=")

(defun gerrit-download:--in-current-repo (changenr)
  (interactive
   (list
    (gerrit--select-change-from-matching-changes
     ;; create a filter that matches only changes for the current project
     ;; and for the selected (if any) branch
     (concat "status:open"
             " project:" (gerrit-get-current-project)
             (car (cl-loop for arg in (transient-args 'gerrit-download-transient) collect
                           (cond ((s-starts-with? "branch=" arg)
                                  (concat " branch:" (s-chop-prefix "branch=" arg)))
                                 ;; TODO add support for other filter options
                                 (t
                                  nil))))))))
  ;; (message "CR: %s, %s" changenr args))
  (gerrit-download--new changenr))

(transient-define-prefix gerrit-download-transient ()
  "Transient used for downloading changes"
  ;; download in current repo (key c: current)
  ;; TODO download in all known projects (key o: all/other)
  ;; download specific branch (depends on project!)

  ["Arguments"
   (gerrit-download:--branch)
  ]
  ["Actions"
   ;; TODO display somewhere the name of the current repo (not sure if
   ;; emacs-transient supports this)
   ("c" "In current repo" gerrit-download:--in-current-repo)
   ;; ("k" "In known repo" gerrit-download:--in-known-repo)
   ])

;; deprecated
(defun gerrit-download (changenr)
  "Download change with CHANGENR from the gerrit server."
  (interactive
   (list
    (gerrit--select-change-from-matching-changes
     (concat "status:open project:"
             (gerrit-get-current-project)))))

  (gerrit--init-accounts)
  (if gerrit-use-gitreview-interface
      (gerrit-download--gitreview changenr)
    (gerrit-download--new changenr)))

(defun gerrit-upload ()
  (interactive)
   (if gerrit-use-gitreview-interface
       (hydra-gerrit-upload/body)

     (condition-case err
         (gerrit-get-changeid-from-current-commit)
       (error
        (gerrit--ensure-commit-msg-hook-exists) ;; create commit-msg hook
        (error (error-message-string err))))

     (call-interactively #'gerrit-upload-transient)))

(provide 'gerrit)
;;; gerrit.el ends here
