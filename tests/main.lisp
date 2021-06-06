(defpackage cl-morse/tests/main
  (:use :cl
        :cl-morse
        :rove))
(in-package :cl-morse/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-morse)' in your Lisp.

(deftest string->morse-test
  (testing "testing the whole alphabet."
    (let* ((input "abcdefghijklmnopqrstuvwxyz. ")
           (expected ".-/-.../-.-./-.././..-./--./..../../.---/-.-/.-../--/-./---/.--./--.-/.-./.../-/..-/...-/.--/-..-/-.--/--..////"))
      (ok (equal expected (cl-morse:string->morse input))
          "The result should be the whole alphabet concatenated.")))
  (testing "signals on unknown character"
    (ok (signals (cl-morse:string->morse "+--") 'cl-morse:unknown-character)))
  (testing "skips unknown characters when using restart"
    (let* ((input "a+b")
           (expected ".-/-.../"))
      (ok (equal expected (cl-morse:string->morse input :skip-unknown-chars t))))))

(deftest morse->string-test
  (testing "testing the whole alphabet."
    (let* ((input ".-/-.../-.-./-.././..-./--./..../../.---/-.-/.-../--/-./---/.--./--.-/.-./.../-/..-/...-/.--/-..-/-.--/--..////")
           (expected "abcdefghijklmnopqrstuvwxyz. "))
      (ok (equal expected (cl-morse:morse->string input)))))
  (testing "signals on unknown character"
    (ok (signals (cl-morse:morse->string ".-/....../-.../") 'cl-morse:unknown-character)))
  (testing "skips unknown characters when using restart"
    (let* ((input ".-/....../-.../")
           (expected "ab"))
      (ok (equal expected (cl-morse:morse->string input :skip-unknown-chars t))))))

;; run tests on compile file / C-c C-k
(run-suite *package*)
