(require 'gerrit-rest)

(ert-deftest test-with-authorization-gh ()
  (let* (
        (gerrit-host "review.gerrithub.io")
        (gerrit-rest-endpoint-prefix "/a")
        (resp (gerrit-rest-sync "GET" nil "/changes/?q=is:open limit:3")))
    (should (= (length resp) 3))))

(ert-deftest test-with-authorization-oh ()
  (let* (
        (gerrit-host "review.opendev.org")
        (gerrit-rest-endpoint-prefix "")
        (resp (gerrit-rest-sync "GET" nil "/changes/?q=is:open limit:3")))
    (should (= (length resp) 3))))

(ert-deftest test-fail-authorization ()
  (let* (
        (gerrit-host "unknown-url")
        (gerrit-rest-endpoint-prefix ""))
    (should-error (gerrit-rest-sync "GET" nil "/changes/?q=is:open limit:3")
                  :type '(error "review.gerrithub2.io/443 Name or service not known"))))

(ert-deftest test-dashboard-getdata-real-gh ()
  (let* (
         (gerrit-host "review.gerrithub.io")
         (gerrit-rest-endpoint-prefix "/a")
         (data (gerrit-dashboard--get-data "is:open limit:4")))
    (should (= (length data) 4))))

(ert-deftest test-dashboard-getdata-real-oh ()
  (let* (
         (gerrit-host "review.opendev.org")
         (gerrit-rest-endpoint-prefix ""))
    ;; is:ignored is not supported by the server (too old gerrit
    ;; version)
    (should-error (gerrit-dashboard--get-data "is:open -is:ignored")
                  :type '(error "tmp"))))
