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

(ert-deftest gerrit-test-alist-rec-get ()
  (should (equal (gerrit--alist-get-recursive 'A 'B '((A . ((B . 3)))))
                 3))
  (should (equal (gerrit--alist-get-recursive 'B 'A '((A . ((B . 3)))))
                 nil))
  (should (equal (gerrit--alist-get-recursive 'A 'B 'C '((A . ((B . 3)))))
                 nil))
  (should (equal (gerrit--alist-get-recursive 'A '((A . ((B . 3)))))
                 '((B . 3))))
  )

;;; gerrit.el-test.el ends here
