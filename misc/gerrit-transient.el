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
;; individual options independent whether the qction was called or not (if a
;; reader is specified, the history parameter needs to be updated for this
;; to work!).

(defun gerrit-upload--action (&optional args)
  (interactive
   (list (transient-args 'gerrit-upload-transient)))
  (message "args: %s" args))

(transient-define-prefix gerrit-upload-transient ()
  "Test Transient Title"
  ["Arguments"
   ;; ("r" "Add reviewer" "reviewers=") ;; TODO multi-value
   (gerrit-upload:--reviewers)
   ;; ("a" "Assignee" "assignee")
   (gerrit-upload:--assignee)
   ("w" "Work in Progress" "wip")
   ("v" "Ready for Review" "ready")
   (gerrit-upload:--topic)
   ;; ("t" "Topic" "topic=")
  ]
  ["Actions"
   ("u" "Upload" gerrit-upload--action)])

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

(defun gerrit-upload:--read-reviewers (prompt _initial-input history)
  (gerrit--init-accounts)
  (let ((val
  (completing-read-multiple
   prompt
   (seq-map #'cdr gerrit--accounts-alist) ;; usernames
   nil
   nil
   nil)))
    (message "%s" val)
    val))
   ;; (mapconcat (lambda (ref)
   ;;              ref)
   ;;            (magit-get-all "notes.displayRef")
   ;;            ",")))

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

  ;; using the history here doesn't have an effect (maybe it doesn, but for
  ;; ivy-completing-read it doesnt)
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
  ;; (message "GRT: %s %s %s" prompt _initial-input (symbol-value history))
  ;; (gerrit-upload-completing-set
  ;;                          "Topic: "
  ;;                          gerrit-upload-topic-history))

  (completing-read
   prompt
   (symbol-value history)
   nil nil nil
   history))

(gerrit-upload-transient)
