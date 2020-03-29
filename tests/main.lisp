(defpackage lassie/tests/main
  (:use :cl
        :lassie
        :rove))
(in-package :lassie/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :lassie)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
