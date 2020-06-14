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
;; * defuns for downloading and uploading a change (`gerrit-upload` and `gerrit-download`)
;;
;;   The git-review command line tool as well as the REST API is used for
;;   these defuns under the hood.
;;
;; * gerrit-dashboard, defun for displaying a dashboard, similiar to the
;;   one of the gerrit webinterface
;;
;; * open-reviews section for the magit-status buffer (`magit-gerrit-insert-status`)
;;
;;     section local keymap:
;;        RET - opens change in browser
;;
;;  See the README.md on the github project page for more information.

;;; Code:

(require 'cl-lib)  ;; for cl-remove-duplicates
(require 'dash)
(require 'hydra)
(require 'magit)
(require 'recentf)
(require 's)

(require 'gerrit-rest)

(defvar gerrit-upload-topic-history nil "List of recently used topic names.")
(defvar gerrit-upload-args-history nil "List of recently used args for git-review cmd.")

(defvar gerrit--accounts-alist nil)

;; these two vars are mainly needed for the hydra-based implementation because
;; I don't know how I can communicate between different heads of the hydra
(defvar gerrit-last-reviewers nil)
(defvar gerrit-last-topic nil)
(defvar gerrit-last-assignee nil)
(defvar gerrit-upload-args nil)
(defvar gerrit-upload-ready-for-review nil)

(defvar gerrit-dashboard-buffer-name "*gerrit-dashboard*" nil)
(defvar gerrit-dashboard-query-alist
  '(("Assigned to me" . "assignee:self (-is:wip OR owner:self OR assignee:self) is:open -is:ignored")
    ("Work in progress" . "is:open owner:self is:wip")
    ("Outgoing reviews" . "is:open owner:self -is:wip -is:ignored")
    ("Incoming reviews" .  "is:open -owner:self -is:wip -is:ignored (reviewer:self OR assignee:self)")
    ("CCed On" . "is:open -is:ignored cc:self")
    ("Recently closed" . "is:closed -is:ignored (-is:wip OR owner:self) (owner:self OR reviewer:self OR assignee:self OR cc:self) limit:15")
    )
  "Query search string that is used for the data shown in the gerrit-dashboard.")

(defalias 'gerrit-dump-variable #'recentf-dump-variable)

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

(defun gerrit--init-accounts ()
  "Intialize `gerrit--accounts-alist`."
  (unless gerrit--accounts-alist
    (setq gerrit--accounts-alist (gerrit-rest--get-gerrit-accounts)))
)

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
       ;; todo simplify the duplicate handling
       (push value ,history) ;; note that we don't need this if the builtin
                             ;; completeing-read is used. Bug in
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
  (setq gerrit-last-assignee
        (completing-read
         "Assignee: "
         (seq-map #'cdr gerrit--accounts-alist) ;; usernames
         nil ;; predicate
         t ;; require match
         nil ;; initial
         nil ;; hist (output only?)
         ;; def
         nil)))

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
    (if (string= "" gerrit-last-assignee)
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
                                 (gerrit-rest--set-assignee changenr gerrit-last-assignee)))
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

(defalias 'gerrit-upload #'hydra-gerrit-upload/body)

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
  "Keymap for `magit-open-reviews' top level section.  The prefix magit- prefix is requred by `magit-insert-section'.")

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

(defun gerrit-get-current-project ()
  "Return the gerrit project name, e.g., 'software/jobdeck'."
  (interactive)
  (let ((origin-url (car
                     (magit-config-get-from-cached-list
                      ;; TODO read remote name from .git-review file
                      "remote.origin.url"))))
    (if (s-starts-with? "https://" origin-url)
        (nth 2 (s-split-up-to "/" origin-url 3 t)) ;; return the endpoint (everything after the 3rd /)
      (s-chop-suffix
       ".git"
       (nth 1 (s-split ":" origin-url))))))



;; dashboard

(defface gerrit-fail
  '((t (:foreground "red4")))
  "Used for negative votes."
  :group 'faces)

(defface gerrit-success
  '((t (:foreground "green4")))
  "Used for positive votes."
  :group 'faces)

(defface gerrit-section
  '((t (:foreground "green4" :weight bold :underline t)))
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

(defun gerrit-dashboard--get-data (expression)
  (gerrit--init-accounts)
  (seq-map (lambda (change)
             (let ((subject (cdr (assoc 'subject change)))
                   (owner (cdr (car (cdr (assoc 'owner change)))))
                   (assignee (cdr (car (cdr (assoc 'assignee change)))))
                   (repo (cdr (assoc 'project change)))
                   (branch (cdr (assoc 'branch change)))
                   (topic (cdr (assoc 'topic change)))
                   (updated (cdr (assoc 'updated change)))
                   (insertions (cdr (assoc 'insertions change)))
                   (deletions (cdr (assoc 'deletions change)))
                   ;; is one of the symbols
                   ;; 'rejected, 'approved, 'disliked or 'recommended (or nil)
                   (CR-vote (car (car
                                  (cdr (assoc 'Code-Review
                                              (cdr (assoc 'labels
                                                          change)))))))
                   ;; is one of the symbols
                   ;; 'approved or 'disliked (or nil)
                   (verified (car (car
                                   (cdr (assoc 'Verified
                                               (cdr (assoc 'labels
                                                           change)))))))
                   )

               `(nil  [,subject
                       ,(alist-get owner gerrit--accounts-alist)
                       ,(or (alist-get assignee gerrit--accounts-alist) "")
                       ,repo
                       ,branch
                       ,(or topic "")
                       ;; TODO convert datetime str to pretty relative time (eg. 3min ago)
                       ;; take a look at  magit-log-format-author-margin (style = age-abbreviated)
                       ,updated
                       ;; TODO finish this
                       ,(if (< (+ deletions insertions) 15) "S" "L")
                       ,(gerrit--combined-level-to-numberstr CR-vote nil)
                       ,(gerrit--combined-level-to-numberstr verified t)
                       ])))
           (gerrit-rest-change-query expression)))

;; (defvar gerrit-dashboard-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "l") 'test-function)
;;     (define-key map (kbd "g") 'gerrit-dashboard) ;; refresh
;;     (define-key map (kbd "a") 'gerrit-rest--set-assignee) ;; refresh
;;    map))

(defvar gerrit-dashboard-columns
  [("Subject" 55)
   ("Owner" 15)
   ("Assignee" 15)
   ("Repo" 24)
   ("Branch" 12)
   ("Topic" 15)
   ("Updated" 12)
   ("Size" 3)
   ("CR" 2)
   ("V" 2)]
  "Column-names and column-sizes of the gerrit dashboard."
  )

(define-derived-mode gerrit-dashboard-mode tabulated-list-mode "gerrit-dashboard"
  "gerrit-dashboard mode"
  (let* ((columns gerrit-dashboard-columns)
        (rows
         (seq-reduce (lambda (acc conscell)
                       (let ((section-data
                              (gerrit-dashboard--get-data (cdr conscell))))
                         (append acc `((nil [,(propertize
                                               (format "%s (%d)" (car conscell) (length section-data))
                                               'face 'gerrit-section)
                                             ;; is there an easier way to add len(columns)-1 times ""?
                                             ,@(seq-map (lambda (x) "") (number-sequence 1 (1- (length columns))))]))
                                 section-data)))
                     gerrit-dashboard-query-alist '())))
    (setq tabulated-list-format columns)
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)))

;;;###autoload
(defun gerrit-dashboard ()
  "Show a dashboard in a new buffer."
  (interactive)
  (switch-to-buffer gerrit-dashboard-buffer-name)
  (gerrit-dashboard-mode))

(provide 'gerrit)
;;; gerrit.el ends here
