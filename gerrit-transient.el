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

(define-transient-command gerrit-upload-transient ()
  "Test Transient Title"
  ["Arguments"
   ("r" "Add reviewer" "reviewers=") ;; TODO multi-value
   ;; ("a" "Assignee" "assignee")
   (gerrit-upload:--assignee)
   ("w" "Work in Progress" "wip")
   ("v" "Ready for Review" "ready")
   (gerrit-upload:--topic)
   ;; ("t" "Topic" "topic=")
  ]
  ["Actions"
   ("u" "Upload" gerrit-upload--action)])

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
  (message "GRA: %s %s %s" prompt _initial-input (symbol-value history))
  (--when-let (magit-completing-read
               prompt
               (magit-list-notes-refnames)
               nil
               nil
               nil ;; default value
               ;; (--when-let (magit-get "core.notesRef")
               ;;   (if (string-prefix-p "refs/notes/" it)
               ;;       (substring it 11)
               ;;     it))
               history)
    it))

(defun gerrit-upload:--read-assignee (prompt _initial-input history)
  (gerrit--init-accounts)
  (gerrit--read-assignee))

(defun gerrit-upload:--read-topic (prompt _initial-input history)
  (message "GRT: %s %s %s" prompt _initial-input (symbol-value history))
  ;; (gerrit-upload-completing-set
  ;;                          "Topic: "
  ;;                          gerrit-upload-topic-history))
  (let ((value (completing-read
                prompt
                (symbol-value history);;nil
                nil nil nil
                history)))
    value))
     ;;            nil
     ;;             nil ;;(car ,history)

     ;;             )))
     ;; ;; (unless (equal "" value)
     ;; ;;   ;; todo simplify the duplicate handling
     ;; ;;   (push value ,history)
     ;; ;;   (setq ,history (cl-remove-duplicates ,history :test 'string=)))
     ;; value))


(gerrit-upload-transient)
