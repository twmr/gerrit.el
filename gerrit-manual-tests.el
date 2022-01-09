(require 'cl-lib)
(require 'gerrit-rest)
(require 'gerrit)


(ert-deftest test-with-authorization-gh ()
  (let* (
        (gerrit-host "review.gerrithub.io")
        (resp (gerrit-rest-sync "GET" nil "/changes/?q=is:open limit:3")))
    (should (= (length resp) 3))))

(ert-deftest test-with-authorization-oh ()
  (let* (
        (gerrit-host "review.opendev.org")
        (resp (gerrit-rest-sync "GET" nil "/changes/?q=is:open limit:3")))
    (should (= (length resp) 3))))

(ert-deftest test-fail-authorization ()
  (let* (
        (gerrit-host "unknown-url"))
    (should-error (gerrit-rest-sync "GET" nil "/changes/?q=is:open limit:3")
                  :type '(error "review.gerrithub2.io/443 Name or service not known"))))

(ert-deftest test-dashboard-getdata-real-gh ()
  (let* (
         (gerrit-host "review.gerrithub.io")
         (data (gerrit-dashboard--get-data "is:open limit:4")))
    (should (= (length data) 4))))

(ert-deftest test-dashboard-getdata-real-oh ()
  (let* (
         (gerrit-host "review.opendev.org"))
    (should (= (length (gerrit-dashboard--get-data "is:open -is:ignored limit:4")) 4))))

(ert-deftest test-dashboard-getchange-metadata-gh ()
  (let* (
         (gerrit-host "review.gerrithub.io")
         (data (seq-map #'gerrit-dashboard--get-change-metadata
                        (gerrit-rest-change-query "is:open limit:3"))))
    (should (= (length data) 3))
    (should (cl-every #'numberp  (seq-map (lambda (change) (alist-get 'number change)) data)))))

;;; gerrit-manual-tests.el ends here
