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

  ;; tests where the keyname is a string of an alist is a string
  (let ((my-alist (gerrit-rest--read-json
                   "{\"key\": \"boo\", \"main\":{\"foo\": 4, \"boo\": 3}}")))

    ;; the value of alist-get 'key is a string, which is used as a key for
    ;; the sub-alist main.
    (should (equal my-alist '((key . "boo") (main (foo . 4) (boo . 3)))))

    ;; (gerrit--alist-get-recursive 'main "boo" my-alist) doesn't work

    (should (equal (gerrit--alist-get-recursive 'main 'boo my-alist) 3))

    (should (equal (gerrit--alist-get-recursive 'main
                             (intern (alist-get 'key my-alist))
                             my-alist) 3)))
  )




;;; gerrit.el-test.el ends here
