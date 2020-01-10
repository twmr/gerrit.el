;;; gerrit.el-test.el --- Tests for gerrit.el -*- lexical-binding: t; -*-

(require 'cl-lib)

(require 'gerrit)


(ert-deftest gerrit-test-get-current-project ()
  (cl-letf (((symbol-function 'magit-config-get-from-cached-list)
             (lambda (&rest _) `("https://review.gerrithub.io/parent/pro1"))))
    (should (equal (call-interactively #'gerrit-get-current-project) "parent/pro1")))

  (cl-letf (((symbol-function 'magit-config-get-from-cached-list)
             (lambda (&rest _) `("git@review.gerrithub.io:parent/pro2.git"))))
    (should (equal (call-interactively #'gerrit-get-current-project) "parent/pro2"))))

;;; gerrit.el-test.el ends here
