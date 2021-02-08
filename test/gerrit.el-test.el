;;; gerrit.el-test.el --- Tests for gerrit.el -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)

(require 'gerrit)


(ert-deftest gerrit-test-get-current-project ()
  (let ((test-pairs
		 '(;; ssh
		   ("ssh://user@review.gerrithub.io/parent/ssh1.git/" . "parent/ssh1")
		   ("ssh://user@review.gerrithub.io:443/parent/ssh2.git" . "parent/ssh2")
		   ("ssh://review.gerrithub.io:443/parent/ssh3" . "parent/ssh3")
		   ("ssh://review.gerrithub.io/parent/ssh4" . "parent/ssh4")
		   ;; git
		   ("git://review.gerrithub.io:443/parent/git1" . "parent/git1")
		   ("git://review.gerrithub.io/parent/git2.git" . "parent/git2")
		   ;; http[s]
		   ("https://review.gerrithub.io/parent/http1" . "parent/http1")
		   ("https://review.gerrithub.io:80/parent/http2" . "parent/http2")
		   ("http://review.gerrithub.io/parent/http3" . "parent/http3")
		   ("http://review.gerrithub.io:80/parent/http4" . "parent/http4")
		   ;; ftp[s]
		   ("ftps://review.gerrithub.io/parent/ftp1" . "parent/ftp1")
		   ("ftps://review.gerrithub.io:80/parent/ftp2.git" . "parent/ftp2")
		   ("ftp://review.gerrithub.io/parent/ftp3.git/" . "parent/ftp3")
		   ("ftp://review.gerrithub.io:80/parent/ftp4" . "parent/ftp4")
		   ;; scp-like
		   ("git@review.gerrithub.io:parent/scp1.git" . "parent/scp1")
		   ("review.gerrithub.io:parent/scp2.git" . "parent/scp2")
		   )))
	(cl-loop for (git-url . project) in test-pairs
			 do
			 (cl-letf (((symbol-function 'magit-config-get-from-cached-list)
						(lambda (&rest _) `(,git-url))))
			   (ert-info ((format "Project for %s should be %s" git-url project))
				 (should (equal (call-interactively #'gerrit-get-current-project) project)))))))

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
