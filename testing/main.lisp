;; A top-level call for running regression tests for IDyOM using the 5AM test framework
;;
;; https://common-lisp.net/project/fiveam/
;;
;; PROTOCOL:
;; * tests are added for each package individually with the file-naming convention: package-tests.lisp
;; * 5am suites are added with the package name and included in the testing::idyom-tests suite
;; * subsuites may be created for each package
;; * all suites are listed below
;;
;; Packages with tests:
;;
;; * resampling
;; --> create-resampling-sets
;; --> output-formatting

(cl:in-package #:testing)

(5am:def-suite idyom-tests)

(defun run-tests (&optional (suite 'idyom-tests))
  ;; resampling package
  (5am:run! suite))
