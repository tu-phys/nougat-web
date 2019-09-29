(defpackage nougat-web/tests/main
  (:use :cl
        :nougat-web
        :rove))
(in-package :nougat-web/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :nougat-web)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
