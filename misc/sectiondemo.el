;; sectiondemo.el --- Example of using gerrit.el -*- lexical-binding: t; -*-
(require 'gerrit)
(require 'magit)

(defun gerrit-section-insert-comments ()
  (magit-insert-section (toplevel) ; without this, I can't toggle the repo
                                        ; sections for some reason.
    (insert "This is a gerrit section demo\n") ;; without this line pressing "1" doesn't collapse all the sections ...
    (cl-loop for changename in '("chg1" "chg2") do
             (magit-insert-section (gerrit-change changename)
               (magit-insert-heading changename)

               (insert "This is some text that describes what this change is about\n")

               (cl-loop for sectionname in '("authorname1" "authorname2" "a3") do
                        (magit-insert-section (gerrit-comments)
                          (magit-insert-heading
                                   (propertize sectionname 'font-lock-face
                                               'magit-section-secondary-heading)
                                   " "
                                   (propertize "PS1" 'face 'gerrit-fail))

                          (magit-insert-section (gerrit-comment (concat "file:" sectionname))
                            (magit-insert-heading "comment1")
                            (insert "text text text\n")

                            )))))
    (insert ?\n)))


;; (add-hook 'magit-status-sections-hook #'gerrit-section-insert-comments t)


(defun gerrit-section-demo ()
  (interactive)
  (let ((buffer (get-buffer-create "*gerrit-section*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (magit-section-mode)
        (gerrit-section-insert-comments))
      (switch-to-buffer-other-window (current-buffer)))))
