;;; gerrit.el --- Gerrit integration @ IMS -*- lexical-binding: t; -*-

;; Author: Thomas Hisch <t.hisch@gmail.com>
;; Maintainer: Thomas Hisch <t.hisch@gmail.com>
;; URL: https://github.com/thisch/gerrit.el
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (hydra "0.15.0") (magit "2.13.1") (s "1.12.0"))
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

;;
;; This package contains
;;
;; * defuns for downloading and uploading a change (`gerrit-upload`, `gerrit-download`)
;;   The command line tool git-review is used for this under the hood.
;; * open-reviews section in magit
;;     The (open) gerrit changes for the current project are queried using the rest API.
;;
;;     section local keymap:
;;        RET - opens change in browser
;; * defun for setting assignee of a gerrit change using rest api `gerrit-rest--set-assignee`
;;
;; TODOS:
;; when uploading a new patchset for a change (via `gerrit-upload`) show votes
;; include votes in  open gerrit review lines
;; parse commit messages and show jira tickets (ret on jira tickets opens them)
;;  where should the jira tickets be displayed?
;; write some testcases
;; rename gerrit-upload to gerrit-change-upload and gerrit-download to gerrit-change-download.

;;; Code:

(require 'cl-lib)  ;; for cl-remove-duplicates
(require 'hydra)
(require 'json)
(require 'magit)
(require 'recentf)
(require 's)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'url-vars)

(require 'gerrit-rest)

(defvar gerrit-upload-topic-history nil "List of recently used topic names.")
(defvar gerrit-upload-args-history nil "List of recently used args for git-review cmd.")

(defvar gerrit--usernames nil)

;; these two vars are mainly needed for the hydra-based implementation because
;; I don't know how I can communicate between different heads of the hydra
(defvar gerrit-last-reviewers nil)
(defvar gerrit-last-topic nil)
(defvar gerrit-last-assignee nil)
(defvar gerrit-upload-args nil)
(defvar gerrit-upload-ready-for-review nil)

(defalias 'gerrit-dump-variable 'recentf-dump-variable)

(defgroup gerrit nil
  "Maintain a menu of recently opened files."
  :version "25.1"
  ;; which group should be used?
  :group 'files)

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

(defcustom gerrit-host nil
  "Hostname of the gerrit instance (without the protocol prefix)."
  :group 'gerrit
  :type 'string)

(defcustom gerrit-change-max-nr-digits 5
  "Number of digits used for displaying gerrit changes."
  :group 'gerrit
  :type 'int)

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

(defcustom gerrit-upload-default-args ""
  "Default args used when calling 'git review' to upload a change."
  :group 'gerrit
  :type 'string)

(defun gerrit-load-lists ()
  "Load a previously saved recent list.
Read data from the file specified by `gerrit-save-file'."
  (interactive)
  (let ((file (expand-file-name gerrit-save-file))
        ;; We do not want Tramp asking for passwords.
        (non-essential t))
    (when (file-readable-p file)
      (load-file file))))

(defmacro gerrit-upload-completing-set (msg history &optional history-excludes)
  `(let* ((reduced-history (-difference ,history ,history-excludes))
          (value (completing-read
                 ,msg
                 reduced-history
                 nil nil nil nil
                 ;; default value set to LRU reviewers value
                 (car reduced-history))))
     (unless (equal "" value)
       ;; todo simplify the duplicate handling
       (push value ,history)
       (setq ,history (cl-remove-duplicates ,history :test 'string=)))
     value))

(defmacro gerrit-upload-completing-set-with-fixed-collection
    (msg collection history &optional history-excludes)
  `(let* ((reduced-history (-difference ,history ,history-excludes))
          (value (completing-read
                  ,msg
                  ,collection
                  nil ;; predicate
                  t ;; require match
                  nil ;; initial input
                  nil ;; history
                  ;; default value set to LRU reviewers value
                  (car reduced-history))))
     (unless (equal "" value)
       ;; todo simplify the duplicate handling
       (push value ,history) ;; note that we don't need this if the builtin
                             ;; completeing-read is used. Bug in
                             ;; ivy-completing-read?
       (setq ,history (cl-remove-duplicates ,history :test 'string=)))
     value))

(defun gerrit-upload-add-reviewer ()
  "Interactively ask for to-be-added reviewer name."
  (interactive)
  (unless gerrit--usernames
    (setq gerrit--usernames (gerrit-rest--get-gerrit-usernames)))

  ;; exclude the ones from the history that have already been added
  (push (gerrit-upload-completing-set-with-fixed-collection
         "Reviewer: "
         gerrit--usernames
         gerrit-last-reviewers)
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
  (unless gerrit--usernames
    (setq gerrit--usernames (gerrit-rest--get-gerrit-usernames)))
  (completing-read
   "Assignee: "
   gerrit--usernames
   nil ;; predicate
   t ;; require match
   nil ;; initial
   nil ;; hist (output only?)
   ;; def
   nil))

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
    ;; (message cmdstr)
    (magit-git-command cmdstr)))

(defhydra hydra-gerrit-upload (:color amaranth ;; foreign-keys warning, blue heads exit hydra
                               :hint nil ;; show hint in the echo area
                               :columns 1
                               :body-pre (progn
                                           (gerrit-load-lists)
                                           (setq gerrit-last-topic "")
                                           (setq gerrit-last-reviewers '())
                                           (setq gerrit-upload-args gerrit-upload-default-args)
                                           (setq gerrit-upload-ready-for-review nil))
                               :after-exit (gerrit-save-lists))
  "
gerrit-upload: (current cmd: %(concat (gerrit-upload-create-git-review-cmd)))
"
  ("r" gerrit-upload-add-reviewer "Add reviewer")
  ("R" gerrit-upload-remove-reviewer "Remove reviewer")
  ;; ("g" gerrit-upload-add-review-group "Add review group")
  ("a" gerrit-upload-set-assignee "Set assignee")
  ("t" gerrit-upload-set-topic "Set topic")
  ("v" gerrit-upload-toggle-ready-for-review "Toggle ready-for-review")
  ("a" gerrit-upload-set-args "Set additional args")
  ("RET" gerrit-upload-run "Run git-reivew" :color blue))

(defalias 'gerrit-upload 'hydra-gerrit-upload/body)

(defun gerrit-download ()
  "Download change from the gerrit server."
  (interactive)
  ;; TODO handle non-zero exit status (see https://stackoverflow.com/questions/23299314/finding-the-exit-code-of-a-shell-command-in-elisp)
  (let ((open-changes (shell-command-to-string "git review -l")))

    ;; remove last two lines
    (setq open-changes (nbutlast (s-lines open-changes) 2))
    ;; (message (s-join "\n" open-changes))
    (let ((changenr (completing-read
                     "Download Change: " open-changes nil nil)))
      (magit-git-command (concat "git review -d "
                                 (car (s-split " " (s-trim changenr))))))))



(defun gerrit-magit-insert-status ()
  (magit-insert-section (open-reviews)
    (magit-insert-heading "Open Gerrit Reviews")
    (dolist (loopvar (gerrit-magit--fetch-open-reviews))
      ;; TODO don't hardcode element indices here
      (let ((changenr (nth 0 loopvar))
            (branch (nth 1 loopvar))
            (topicname (nth 2 loopvar))
            (subject (nth 3 loopvar)))
        (magit-insert-section (open-reviews-issue loopvar t)
          (magit-insert-heading
            ;; TODO determine gerrit-change-nr-digits automatically here
            (format (format "%%%ds %%%ds %%s" gerrit-change-max-nr-digits 40)
                    (format "#%d" changenr)
                    (concat
                     "("
                     (if (< 0 (length topicname))
                         (propertize (concat topicname "@") 'face '(:foreground "green"))
                       "")
                     (propertize branch 'face '(:foreground "red"))
                     ")")
                    subject)))))
    (insert ?\n)))

;; don't rename this var, as it is required for magit-sections (see
;; magit-insert-section)
(defvar magit-open-reviews-issue-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'gerrit-magit-open-reviews--open-gerrit-change)
    map)
  "Keymap for `magit-open-reviews' top-level section.")

(defun gerrit-magit-open-reviews--open-gerrit-change ()
  (interactive)
  (browse-url (format
               "https://%s/c/%s"
               gerrit-host
               ;; TODO change code s.t. s-chop-prefix is not needed
               ;; TODO what does (oref (magit-current-section) value) return?
               (s-chop-prefix "#"
                              ;; TOOD avoid using prin1-to-string?!?
                              (prin1-to-string (nth 0 (oref (magit-current-section) value)))))))
  ;; (message (prin1-to-string (nth 0 (oref (magit-current-section) value)))))

(defun gerrit-get-current-project ()
  "Return the gerrit project name, e.g., 'software/jobdeck'."
  (interactive)
  (s-chop-suffix
   ".git"
   (nth 1 (s-split ":" (nth 0
                            (magit-config-get-from-cached-list
                             "remote.origin.url"))))))

(defun gerrit-magit--fetch-open-reviews ()
  "Return a sequence of (number branch topic subject)."
  (interactive)
  ;; we need the following information:
  ;; changenr, version, name, CR/V, assignee, topic, fixes/related ticket
  ;; sort by modification-date?
  (condition-case nil
      (mapcar (lambda (change) (seq-map (lambda (fieldname) (cdr
                                  (assoc fieldname (cdr change))))
                          (list '_number 'branch 'topic 'subject)))
              (gerrit-rest-open-reviews-for-project (gerrit-get-current-project)))
    (error '())))



(provide 'gerrit)
;;; gerrit.el ends here
