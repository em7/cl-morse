(defpackage cl-morse/tests/main
  (:use :cl
        :cl-morse
        :fiveam))
(in-package :cl-morse/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-morse)' in your Lisp.

(def-suite all-tests)

(in-suite all-tests)

(test string->morse-test-whole-alphabet-test
  "testing the whole alphabet."
  (let* ((input "abcdefghijklmnopqrstuvwxyz. ")
         (expected ".-/-.../-.-./-.././..-./--./..../../.---/-.-/.-../--/-./---/.--./--.-/.-./.../-/..-/...-/.--/-..-/-.--/--..////"))
    (is (equal expected (cl-morse:string->morse input))
        "The result should be the whole alphabet concatenated.")))

(test string->morse-signal-unknown-test
  "signals on unknown character"
  (signals cl-morse:unknown-character
      (cl-morse:string->morse "+--") ))

(test string->morse-skip-unknown-using-restart-test
  "skips unknown characters when using restart"
  (let* ((input "a+b")
         (expected ".-/-.../"))
    (is (equal expected (cl-morse:string->morse input :skip-unknown-chars t)))))

(test morse->string-test-whole-alphabet-test
  "testing the whole alphabet."
  (let* ((input ".-/-.../-.-./-.././..-./--./..../../.---/-.-/.-../--/-./---/.--./--.-/.-./.../-/..-/...-/.--/-..-/-.--/--..////")
         (expected "abcdefghijklmnopqrstuvwxyz. "))
    (is (equal expected (cl-morse:morse->string input)))))

(test morse->string-signal-unknown-character-test
  "signals on unknown character"
  (signals cl-morse:unknown-character
    (cl-morse:morse->string ".-/....../-.../")))

(test morse->string-skip-unknown-using-restart-test
  "skips unknown characters when using restart"
  (let* ((input ".-/....../-.../")
         (expected "ab"))
    (is (equal expected (cl-morse:morse->string input :skip-unknown-chars t)))))


