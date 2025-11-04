;;; gerrit.el --- Gerrit client -*- lexical-binding: t; -*-

;; Author: Thomas Hisch <t.hisch@gmail.com>
;; Maintainer: Thomas Hisch <t.hisch@gmail.com>
;; URL: https://github.com/thisch/gerrit.el
;; Version: 0.1
;; Package-Requires: ((emacs "28.2") (magit "2.13.1") (s "1.12.0") (dash "0.2.15"))
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
;; * (`gerrit-upload` and `gerrit-download`)
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
(require 'magit)
(require 'magit-margin) ;; for magit--age
(require 'magit-section)
(require 's)
(require 'subr-x) ;; for string-empty-p

(require 'gerrit-rest)

(defvar gerrit--accounts-alist nil)

(defvar gerrit-dashboard-buffer-name "*gerrit-dashboard*" nil)
(defvar gerrit-dashboard-query-alist
  '(("Has draft comments" . "has:draft")
    ("Your turn" . "attention:self")
    ("Work in progress" . "is:open owner:self is:wip")
    ("Outgoing reviews" . "is:open owner:self -is:wip")
    ("Incoming reviews" . "is:open -owner:self -is:wip reviewer:self")
    ("CCed on" . "is:open cc:self")
    ("Recently closed" . "is:closed (-is:wip OR owner:self) (owner:self OR reviewer:self OR cc:self) limit:15"))
  "Query search string that is used for the data shown in the `gerrit-dashboard'.")

(defgroup gerrit nil
  "Maintain a menu of recently opened files."
  :version "25.1"
  ;; which group should be used?
  :group 'files)

(defcustom gerrit-host nil
  "Hostname of the gerrit instance (without the protocol prefix)."
  :group 'gerrit
  :type 'string)

(defcustom gerrit-use-ssl t
  "Whether or not to connect to gerrit instance with SSL or not."
  :group 'gerrit
  :type 'boolean)

(defcustom gerrit-change-max-nr-digits 5
  "Number of digits used for displaying gerrit changes."
  :group 'gerrit
  :type 'int)

(defcustom gerrit-dashboard-attention-icon "!"
  "Character or icon used for the attention-set indicator.

This attention-set indicator is prepended in the dashboard to the
user names."
  :group 'gerrit
  :type 'string)

(defun gerrit-get-accounts-alist ()
  "Intialize `gerrit--accounts-alist`."
  (unless gerrit--accounts-alist
    (message "Fetching gerrit accounts ...")
    (setq gerrit--accounts-alist (gerrit-rest--get-gerrit-accounts))
    (message "Fetched gerrit accounts (len=%d)" (length gerrit--accounts-alist)))
  gerrit--accounts-alist)

(defun gerrit-get-usernames ()
  "Get all known usernames known to the gerrit server."
  (seq-map (lambda (account-entry) (alist-get 'username (cdr account-entry)))
           (gerrit-get-accounts-alist)))

(defun gerrit--get-protocol ()
  (if gerrit-use-ssl
      "https://"
    "http://"))

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
  '(number owner branch subject)
  "List of shown columns for change-selection.

  List of columns that should be displayed in functions that ask
  the user to select a change from a list of changes.

  Currently supported columns are:
  \='number (the change number)
  \='owner (the change owner)
  \='branch (the branch of the change)
  \='subject (the subject of the commit msg)
  \='project (the project name)")

;; TODO introduce a function?
(defcustom gerrit-project-to-local-workspace-alist nil
  "This alist can be used for specifying the \='known\=' gerrit projects.

The alist is needed for determining the workspace directory for
certain gerrit projects.

Each element is a list comprising ((PROJECT BRANCH) WORKSPACE) ..."
  :group 'gerrit
  :type '(alist :key-type (list (symbol :tag "Project")
                                (string :tag "Branch"))
                :value-type (string :tag "Workspace Directory")))
;; Example:
;; (setq gerrit-project-to-local-workspace-alist
;;   '(
;;     (("software/pro1" "branch1") "~/sandbox/pro1")
;;     (("software/pro2" "branch2") "~/sandbox/pro2")
;;   ))

(defcustom gerrit-interesting-open-changes-filter "is:open"
  "Filter string used for querying gerrit changes.

If you are interested only in the changes for certain projects,
you can use \='is:open (project:A OR project:B OR project:C)\='"
  :group 'gerrit
  :type 'string)

(defun gerrit-download-format-change (change)
  (let (columns)
    ;; can this be implemented in an easier way?
    (when (member 'number gerrit-change-singleline-columns)
      (push (propertize (number-to-string
                         (alist-get '_number change)) 'face 'magit-hash) columns))
    (when (member 'owner gerrit-change-singleline-columns)
      (push (propertize
             (format "%-20s"
                     ;; TODO abbreviate if author name longer
                     ;; than 20chars
                     (gerrit--alist-get-recursive
                      (gerrit--alist-get-recursive 'owner '_account_id change)
                      'name
                      (gerrit-get-accounts-alist)))
             'face 'magit-log-author) columns))
    (when (member 'project gerrit-change-singleline-columns)
      (push (format "%-28s" (alist-get 'project change)) columns))
    (when (member 'branch gerrit-change-singleline-columns)
      (push (propertize (format "%-7s" (alist-get 'branch change))
                        'face 'magit-branch-remote) columns))
    (when (member 'subject gerrit-change-singleline-columns)
      (push (propertize (alist-get 'subject change) 'face 'magit-section-highlight) columns))
    (s-join " " (nreverse columns))))

(defun gerrit-download--get-refspec (change-metadata)
  "Return the refspec of a gerrit change from CHANGE-METADATA.

This refspec is a string of the form \='refs/changes/xx/xx/x\='."
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

(defun gerrit--download-change (change-metadata workspace-directory)
  ;; to see what git-review does under the hood - see:
  ;; strace -z -f -e execve git-review -d 3591
  (let* ((change-nr (alist-get '_number change-metadata))
         (change-branch (alist-get 'branch change-metadata))
         (change-topic (or (alist-get 'topic change-metadata)
                           (number-to-string change-nr)))
         ;; should we use the username for the branch names or the
         ;; displayname if it is set??
         (change-owner (gerrit--alist-get-recursive
                        (gerrit--alist-get-recursive
                         'owner '_account_id change-metadata)
                        'username
                        (gerrit-get-accounts-alist)))
         (local-branch (format "review/%s/%s"
                               ;; change-owner is 'escaped' by
                               ;; git-review (all non-alphanumeric
                               ;; characters are replaced by _:
                               ;; git-review uses re.sub(r'\W+', "_",
                               ;; ownername), which was introduced
                               ;; 2011 (commit 08bd9c)). It could be
                               ;; that this was done to avoid problems
                               ;; with white-space in branch names.
                               (replace-regexp-in-string "\\W+" "_" change-owner)
                               change-topic)))

    ;; This next (async) call ensures that the authorization works
    ;; (e.g. if ssh-add was not called) this async call runs
    ;; magit-process-password-prompt-regexps (used in magit-process-filter)
    ;; which is called in magit-start-process
    ;; (see https://github.com/magit/magit/issues/4323)
    (let ((default-directory workspace-directory))
      (magit-run-git-async "fetch"
                           (gerrit-get-remote)
                           (gerrit-download--get-refspec change-metadata)))

    (set-process-sentinel
     magit-this-process
     (lambda (process event)
       (when (memq (process-status process) '(exit signal))
         (let ((default-directory workspace-directory))
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
                   ;; since `local-branch' exists, `gerrit--get-tracked' never returns nil
                   (seq-let (tracked-remote tracked-branch) (gerrit--get-tracked local-branch)
                     (unless (and (equal tracked-remote (gerrit-get-remote))
                                  (equal tracked-branch change-branch))
                       (magit-refresh)
                       (error "Branch tracking incompatibility: Tracking %s/%s instead of %s/%s"
                              tracked-remote tracked-branch
                              (gerrit-get-remote) change-branch)))
                   ;; TODO autostash support
                   ;; (magit-git "stash" "create" "download-via-gerrit.el")
                   ;; -B resets the branch to FETCH_HEAD in our case.
                   (magit-git "checkout" "-B" local-branch "FETCH_HEAD"))

               ;; TODO autstash support
               ;; no branch exists that can be reused and updated -> create
               ;; a new one.
               (magit-git "checkout" "-b" local-branch "FETCH_HEAD")
               ;; set upstream here (see checkout_review function in cmd.py)
               ;; this upstream branch is needed for rebasing
               (magit-git "branch"
                          "--set-upstream-to"
                          (format "%s/%s" (gerrit-get-remote) change-branch)
                          local-branch))
             (magit-refresh))))))))

(defun gerrit-download--new (changenr)
  "Download change CHANGENR from the gerrit server using REST interface."
  (let ((change-metadata (car (gerrit-rest-change-query changenr))))
    ;; the return value of `gerrit-rest-change-query` contains the
    ;; current revision, but not the one of `gerrit-rest-change-get`.
    ;; TODO default-directory is not really the workspace directory
    (gerrit--download-change change-metadata default-directory)))

(defun gerrit--ensure-commit-msg-hook-exists ()
  "Create a commit-msg hook, if it doesn't exist."
  (let ((hook-file
         (concat (magit-gitdir) (convert-standard-filename "hooks/commit-msg"))))
    (unless (file-exists-p hook-file)
      (message "downloading commit-msg hook file")
      (url-copy-file
       (concat (gerrit--get-protocol) gerrit-host  "/tools/hooks/commit-msg") hook-file)
      (set-file-modes hook-file #o755))))

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

  (let (push-opts
        no-verify
        (remote (gerrit-get-remote))
        (refspec (gerrit-upload--get-refspec)))
    ;; there are a bunch of push options that are supported by gerrit:
    ;; https://gerrit-review.googlesource.com/Documentation/user-upload.html#push_options

    ;; I don't like this handling of transient-args, maybe transient can
    ;; pass alists to gerrit-upload--action instead of a list of strings

    ;; TODO use code from https://github.com/raxod502/apheleia/pull/56/files
    (cl-loop for arg in args do
             (cond ((s-starts-with? "reviewers=" arg)
                    (cl-loop for reviewer in (s-split "," (s-chop-prefix "reviewers=" arg)) do
                             ;; TODO check that reviewers are valid (by checking that all
                             ;; reviewers don't contain a white-space)
                             (push (concat "r=" reviewer) push-opts)))
                   ((s-starts-with? "topic=" arg)
                    (push  arg push-opts))
                   ((string= "ready" arg)
                    (push "ready" push-opts))
                   ((string= "wip" arg)
                    (push "wip" push-opts))
                   ((string= "--no-verify" arg)
                    (setq no-verify t))))

    (when push-opts
      (setq refspec (concat refspec "%" (s-join "," push-opts))))

    ;; is there an easier way for conditinally skipping "--no-verify"
    ;; from the arguement list for gerrit-push-and-assign?
    (let ((push-args))
      (when no-verify
        (push "--no-verify" push-args))
      (push remote push-args)
      (push (concat "HEAD:" refspec) push-args)

      (apply #'magit-run-git-async
             "push"
             ;; don't error when encountering local tags, which
             ;; are absent from gerrit.
             "--no-follow-tags"
             (nreverse push-args)))))

(transient-define-prefix gerrit-upload-transient ()
  "Transient used for uploading changes to gerrit"
  ["Arguments"
   (gerrit-upload:--reviewers)
   ("w" "Work in Progress" "wip")
   ("v" "Ready for Review" "ready")
   (gerrit-upload:--topic)
   ("-n" "Disable pre-push hooks" "--no-verify")
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
  (when-let* ((value (oref obj value)))
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
  ;; FIXME the sorting order here seems to be different than the one used in
  ;; completing-read! Maybe this is just an ivy issue
  (completing-read-multiple
   prompt
   (gerrit-get-usernames)
   nil
   nil
   nil))

(transient-define-argument gerrit-upload:--topic ()
  :description "Topic"
  :class 'transient-option
  :key "t"
  :argument "topic="
  :reader 'gerrit-upload:--read-topic)

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
  "Show all open gerrit reviews.

When called in the magit-status-section via `magit-status-section-hook'
all open gerrit review are shown in the magit status buffer."

  (when-let* ((fetched-reviews (condition-case nil
                                   (gerrit-rest-open-reviews-for-project
                                    (gerrit-get-current-project))
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
               "%s%s/c/%s"
               (gerrit--get-protocol)
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
  (or (when-let* ((upstream-branch (magit-get-upstream-branch)))
        (cadr (s-split "/" upstream-branch)))
      (magit-git-string "config" "-f" (expand-file-name ".gitreview" (magit-toplevel))
                        "--get" "gerrit.defaultbranch")
      (when-let* ((upstream-branch (magit-read-upstream-branch
                                    nil
                                    (concat "No upstream branch is configured, please specify one "
                                            "(starting with the remote)"))))
        (magit-set-upstream-branch (magit-get-current-branch) upstream-branch)
        (cadr (s-split "/" upstream-branch)))))

(defun gerrit-get-current-project ()
  "Return the gerrit project name, e.g.,\='software/jobdeck\='."
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

(defun gerrit-get-changeid-from-commit (&optional rev)
  "Determine the change-id from the commit REV.

A string like the following is returned:
I8473b95934b5732ac55d26311a706c9c2bde9940"
  (let* (
         (commit-message-lines (magit-git-lines "log" "-1" "--pretty=%B" rev))
         (change-id-line
          ;; in the case of cherry-picks, the Change-Id line may not be the
          ;; last line of the commit message. Therefore, iterate over all
          ;; lines of the commit until a match is found.
          (cl-some (lambda (line) (and (s-starts-with? "Change-Id: " line) line))
                   commit-message-lines)))

    (unless change-id-line
      (error "Commit message doesn't end with a change-id"))
    (s-chop-prefix "Change-Id: " change-id-line)))

(defun gerrit-get-unique-changeid-from-commit (&optional rev)
  "Determine the unique change-id from the commit at REV.

If no revision is provided, the change-id from HEAD is returned.

A string like the following is returned:
myProject~master~I8473b95934b5732ac55d26311a706c9c2bde9940"
  (let ((branch (substring-no-properties (gerrit-get-upstream-branch)))
        (project (substring-no-properties (gerrit-get-current-project))))
    (url-hexify-string (concat
                        project
                        "~"
                        branch
                        "~"
                        (gerrit-get-changeid-from-commit rev)))))



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

      ;; maybe when-let* can be removed, since all commit
      ;; messages are multiline (they include a Change-Id line)
      (when-let* ((commit-msg-body
                   (cdr (s-split-up-to "\n" latest-commit-message 1))))
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
  ;; The last argument in every entry of the list is used to turn on
  ;; sorting (see variable docstring of `tabulated-list-format'.
  [("Number" 8 t)
   ("Subject" 55 t)
   ("Status" 10 t)
   ("Owner" 15 t)
   ("Reviewers" 25 nil)
   ;; ("CC" 15 nil)
   ("Repo" 24 t)
   ("Branch" 12 t)
   ("Topic" 15 t)
   ("Updated" 8 t)
   ("SZ" 3 t)
   ("CR" 2 t)
   ("V" 2 t)]
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
  '((t (:inherit magit-section-heading)))
  "Used for the section names in the dashboard."
  :group 'faces)

(defun gerrit--code-review-label-to-numberstr (code-review-label)
  (or
   (pcase code-review-label
     ('approve (propertize "✔" 'face 'gerrit-success))
     ('recommended (propertize "+1" 'face 'gerrit-success))
     ('disliked (propertize "-1" 'face 'gerrit-fail))
     ('rejected (propertize "-2" 'face 'gerrit-fail)))
   ""))

(defun gerrit--verify-label-to-numberstr (verify-label)
  (or
   (pcase verify-label
     ('approved (propertize "+1" 'face 'gerrit-success))
     ('rejected (propertize "❌" 'face 'gerrit-fail)))
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
  "Convert a json object for CHANGE into an alist.

An object returned by the `gerrit-rest-change-query' is converted into an
alist."
  `((number . ,(alist-get '_number change)) ;; int
    (subject . ,(alist-get 'subject change)) ;; string
    (status . ,(alist-get 'status change)) ;; string
    ;; alist-get 'owner => (_account_id . 1017133)
    (owner . ,(gerrit--alist-get-recursive 'owner '_account_id change))

    ;; all account-ids of all users in the attention set
    (attention-set . ,(seq-map (lambda (attention-entry)
                                 (gerrit--alist-get-recursive 'account '_account_id
                                                              (cdr attention-entry)))
                               (alist-get 'attention_set change)))
    (reviewers . ,(seq-map (lambda (account_info)
                             (alist-get '_account_id account_info))
                           (gerrit--alist-get-recursive 'reviewers 'REVIEWER change)))
    (cc . ,(seq-map (lambda (account_info)
                      (alist-get '_account_id account_info))
                    (gerrit--alist-get-recursive 'reviewers 'CC change)))
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

(defun gerrit-dashboard--button-open-change (&optional button)
  (interactive)
  ;; this is a version of gerrit-dashboard-open-change, but just for buttons
  ;; displayed inside the dashboard
  (gerrit-rest-change-patch (button-get button 'change-id)))

(defun gerrit-dashboard--button-open-owner-query (&optional button)
  (interactive)
  (gerrit-query (concat "owner:" (button-get button 'owner))))

(defun gerrit-dashboard--button-open-branch-query (&optional button)
  (interactive)
  (gerrit-query (concat "branch:" (button-get button 'branch))))

(defun gerrit-dashboard--button-open-repo-query (&optional button)
  (interactive)
  (gerrit-query (concat "project:" (button-get button 'repo))))

(defun gerrit-dashboard--button-open-topic-query (&optional button)
  (interactive)
  (gerrit-query (concat "topic:" (button-get button 'topic))))

(defun gerrit-dashboard--cell-formatter (change-metadata column-name)
  (pcase column-name
    ("Number" (let ((change-id (number-to-string (alist-get 'number change-metadata))))
                `(,(propertize change-id  'face 'magit-hash)
                  change-id ,change-id
                  follow-link t
                  ;; for this to work, an optional button parameter is needed
                  action gerrit-dashboard--button-open-change)))
    ("Subject" (propertize (alist-get 'subject change-metadata) 'face 'magit-section-highlight))
    ("Status" (if (eq (alist-get 'mergeable change-metadata) :json-false)
                  (propertize "Merge conflict" 'face 'gerrit-fail)
                (if (alist-get 'wip change-metadata)
                    "WIP"
                  (let ((status (alist-get 'status change-metadata)))
                    (pcase status
                      ("NEW" "-")
                      ("MERGED" (propertize "Merged" 'face 'gerrit-success))
                      ("ABANDONED" "Abandoned")
                      (_ ""))))))
    ("Owner" (let* ((owner-account-id  (alist-get 'owner change-metadata))
                    (part-of-attention-set (memq owner-account-id
                                                 (alist-get 'attention-set change-metadata))))
               (if-let* ((owner (alist-get owner-account-id (gerrit-get-accounts-alist))))
                   `(,(propertize (concat (if part-of-attention-set
                                              gerrit-dashboard-attention-icon "")
                                          (alist-get 'name owner))
                                  'face 'magit-log-author)
                     owner ,(alist-get 'username owner)
                     follow-link t
                     action gerrit-dashboard--button-open-owner-query)
                 ;; empty owner
                 "")))
    ("Reviewers" (let ((attention-set (alist-get 'attention-set change-metadata))
                       (owner-account-id (alist-get 'owner change-metadata)))
                   ;; TODO exclude the owner from the reviewers
                   (if-let*
                       ((reviewers
                         (delq nil
                               (seq-map
                                (lambda (reviewer-account-id)
                                  (unless (eq reviewer-account-id owner-account-id)
                                    ;; don't display the owner in the reviewers

                                    (let* ((part-of-attention-set
                                            (memq reviewer-account-id attention-set))
                                           (first-name
                                            (car
                                             (split-string
                                              (alist-get 'name (alist-get
                                                                reviewer-account-id
                                                                (gerrit-get-accounts-alist)))))))
                                      (if part-of-attention-set
                                          (propertize (concat
                                                       gerrit-dashboard-attention-icon
                                                       first-name)
                                                      'face 'magit-log-author)
                                        first-name))))
                                (alist-get 'reviewers change-metadata)))))
                       ;; TODO create multiple links (one for each reviewer)
                       ;;(propertize (s-join " " reviewers) 'face 'magit-log-author)
                       (s-join " " reviewers)
                     ;; empty reviewers (not clickable)
                     "")))
    ("CC" (if-let* ((reviewers
                     (seq-map
                      (lambda (reviewer-info)
                        ;; return a real name of a reviewer in CC
                        (alist-get 'name (alist-get reviewer-info
                                                    (gerrit-get-accounts-alist))))
                      (alist-get 'cc change-metadata))))
              (propertize (s-join " " reviewers) 'face 'magit-log-author)
            ""))
    ("Repo" (let ((repo (alist-get 'repo change-metadata)))
              `(,repo
                repo ,repo
                follow-link t
                action gerrit-dashboard--button-open-repo-query)))
    ("Branch" (let ((branch (alist-get 'branch change-metadata)))
                `(,(propertize branch 'face 'magit-branch-remote)
                  branch ,branch
                  follow-link t
                  action gerrit-dashboard--button-open-branch-query)))
    ("Topic" (if-let* ((topic (alist-get 'topic change-metadata)))
                 `(,(propertize topic 'face 'magit-tag)
                   topic , topic
                   follow-link t
                   action gerrit-dashboard--button-open-topic-query)
               ;; empty topic
               ""))
    ("Updated" (gerrit--format-abbrev-date (alist-get 'updated change-metadata)))
    ("SZ"
     ;; TODO finish this
     (if (< (+ (alist-get 'deletions change-metadata)
               (alist-get 'insertions change-metadata)) 15) "S" "L"))
    ("CR" (gerrit--code-review-label-to-numberstr (alist-get 'CR-vote change-metadata)))
    ("V" (gerrit--verify-label-to-numberstr (alist-get 'verified change-metadata)))))

(defun gerrit-dashboard--change-metadata-2-entry (change-metadata)
  ;; iterate over the columns and use pcase to determine the value then use
  ;; seq-into 'vector to convert the list to a vector
  (seq-into (seq-map (lambda (elt)
                       (gerrit-dashboard--cell-formatter change-metadata (car elt)))
                     gerrit-dashboard-columns)
            'vector))

(defun gerrit-dashboard--get-data (expression)
  "Return a list with \"tabulated-list-entries\" matching a query EXPRESSION."
  (seq-map (lambda (change)
             `(nil ,(gerrit-dashboard--change-metadata-2-entry
                     (gerrit-dashboard--get-change-metadata change))))
           (gerrit-rest-change-query expression)))

(defun gerrit-dashboard--entry-number ()
  "Return the change number as a string of a change under point."
  (interactive)
  (car (aref (tabulated-list-get-entry) 0)))

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
               "%s%s/c/%s"
               (gerrit--get-protocol)
               gerrit-host
               (gerrit-dashboard--entry-number))))

(defun gerrit-dashboard-download-change ()
  "Download the change under point into the workspace of the project.

This function requires that the project of the change is cloned
locally and is referenced in
`gerrit-project-to-local-workspace-alist'."
  (interactive)
  (gerrit-download:--in-known-repo (gerrit-dashboard--entry-number)))

(defun gerrit-dashboard-open-change ()
  "Open the patch of the change under point in a new buffer."
  (interactive)
  (gerrit-rest-change-patch (gerrit-dashboard--entry-number)))

(defun gerrit-dashboard-open-topic ()
  "Open the topic information of a topic under point in a new buffer."
  (interactive)
  (gerrit-section-topic-info (gerrit-dashboard--topic)))

(defun gerrit-dashboard-set-verified-vote-topic ()
  (interactive)
  ;; TODO interactively ask for vote + message
  (gerrit-rest-topic-set-verified-vote (gerrit-dashboard--topic) "+1" ""))

(defun gerrit-dashboard-set-cr-vote-topic ()
  (interactive)
  ;; TODO interactively ask for vote + message
  (gerrit-rest-topic-set-cr-vote (gerrit-dashboard--topic) "+2" ""))

(defun gerrit-dashboard--get-list-entries ()
  "Get the all entries used for \"tabulated-list-entries\"."
  (seq-reduce (lambda (acc conscell)
                (let ((section-data
                       (gerrit-dashboard--get-data (cdr conscell))))
                  ;; don't show header line if length of
                  ;; gerrit-dashboard-query-alist is 1 and (car conscell) is
                  ;; nil
                  (if (car conscell)
                      (append acc `((nil [,(propertize
                                            (format "%s (%d)" (car conscell) (length section-data))
                                            'face 'gerrit-section)
                                          ;; is there an easier way to add len(columns)-2 times ""?
                                          ,@(seq-map (lambda (_) "") (number-sequence
                                                                      1 (1- (length gerrit-dashboard-columns))))]))
                              section-data)
                    ;; don't display a header line
                    (append acc section-data))))
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

(defun gerrit-dashboard-edit-query ()
  (interactive)
  ;; TODO generalize this, currently the determination of the query string
  ;; only works in dashboards with a single section.
  (gerrit-query (read-string "Enter query string: " (cdar gerrit-dashboard-query-alist))))

(defvar gerrit-dashboard-mode-map
  ;; TODO convert this into a transient
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") 'gerrit-dashboard-assign-change)
    (define-key map (kbd "A") 'gerrit-dashboard-assign-change-to-me)
    (define-key map (kbd "g") 'gerrit-dashboard--refresh--and-point-restore)
    (define-key map (kbd "o") 'gerrit-dashboard-browse-change)
    (define-key map (kbd "d") 'gerrit-dashboard-download-change)
    (define-key map (kbd "RET") 'gerrit-dashboard-open-change)
    (define-key map (kbd "t") 'gerrit-dashboard-open-topic)
    ;; TODO completion would be extremely nice
    (define-key map (kbd "e") 'gerrit-dashboard-edit-query)
    ;; votes
    (define-key map (kbd "V") 'gerrit-dashboard-set-verified-vote-topic)
    (define-key map (kbd "C") 'gerrit-dashboard-set-cr-vote-topic)
    ;; TODO submit

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

(defun gerrit-query (query)
  "Perform a query QUERY and display it in a dashboard buffer."
  ;; TODO offer completion in interactive ...
  ;; TODO offer a list of candidates (history)
  (interactive "sEnter a query string: ")
  (switch-to-buffer (format "gerrit:%s" query))
  (setq gerrit-dashboard-query-alist
        ;; if car is nil gerrit.el will not display a section line
        `((nil . ,(concat query " limit:50"))))
  (gerrit-dashboard-mode))



(defun gerrit--select-change-from-matching-changes (search-string)
  ;; see https://gerrit-review.googlesource.com/Documentation/user-search.html
  ;; clients can let-bind `gerrit-change-singleline-columns'
  (let* ((open-changes (seq-map #'gerrit-download-format-change
                                (gerrit-rest-change-query
                                 (or search-string "is:open"))))
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
  "Download a gerrit change CHANGENR for the current project.

The download of the change is performed in the workspace of the
current project."
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
  (gerrit-download--new changenr))

(defun gerrit-download:--in-known-repo (changenr)
  "Download a gerrit change CHANGENR for a known project.

The download of the change is performed in the corresponding
workspace of the project."
  (interactive
   (list
    (let
        ((gerrit-change-singleline-columns '(number owner branch project subject)))
      (gerrit--select-change-from-matching-changes
       ;; TODO add support for selecting branches (what if
       ;; gerrit-interesting-open-changes-filter already contains
       ;; branch:...?)
       gerrit-interesting-open-changes-filter))))

  ;; 1) get change metadata
  ;; 2) determine workspace directory (based on branch and projectname)
  ;; 3) switch to workspace
  ;; 4) download change
  (let* ((change-metadata (car (gerrit-rest-change-query changenr)))
         (project-name (alist-get 'project change-metadata))
         (branch (alist-get 'branch change-metadata))
         (workspace-directory (or (cadr (assoc (list project-name branch)
                                               gerrit-project-to-local-workspace-alist))
                                  ;; TODO completion + write them to file
                                  (read-directory-name
                                   (format "Enter directory for project '%s': "
                                           project-name))))

         (default-directory workspace-directory))
    ;; (message "changeinfo: name: %s, branch: %s -> workspace: %s"
    ;;          project-name branch workspace-directory)
    (gerrit--download-change change-metadata workspace-directory)))

(transient-define-prefix gerrit-download-transient ()
  "Transient used for downloading changes"
  ;; download in current repo (key c: current)
  ;; download in all known projects (key k: known)
  ;; download specific branch (depends on project!)

  ["Arguments"
   (gerrit-download:--branch)
   ]
  ["Actions"
   ;; TODO display somewhere the name of the current repo (not sure if
   ;; emacs-transient supports this)
   ("c" "In current repo" gerrit-download:--in-current-repo)
   ("k" "In known repo" gerrit-download:--in-known-repo)
   ])

;; deprecated
(defun gerrit-download (changenr)
  "Download change with CHANGENR from the gerrit server."
  (interactive
   (list
    (gerrit--select-change-from-matching-changes
     (concat "status:open project:"
             (gerrit-get-current-project)))))

  (gerrit-download--new changenr))

(defun gerrit-upload ()
  (interactive)

  ;; Sanity checks:
  ;; * Check if the latest commit contains a ChangeId in the commit msg.
  ;; * Check if the sha1 of the latest PS on the server is the same as
  ;;   the SHA1 of the local HEAD.
  (let* ((changeid
          (condition-case err
              (gerrit-get-unique-changeid-from-commit "HEAD")
            (error
             (gerrit--ensure-commit-msg-hook-exists) ;; create commit-msg hook
             (error (error-message-string err)))))
         (change-info
          (condition-case nil
              (gerrit-rest-get-change-info changeid)
            (error nil)))
         (remote-revision (alist-get 'current_revision change-info))
         (current-revision (magit-rev-parse "HEAD")))

    (when (and remote-revision current-revision)
      ;; (message "Remote revision: %s\nCurrent revision: %s"
      ;;  remote-revision current-revision)
      (when (string= remote-revision current-revision)
        (error "The remote change/relation-chain is up to date."))))

  (call-interactively #'gerrit-upload-transient))

(provide 'gerrit)
;;; gerrit.el ends here
