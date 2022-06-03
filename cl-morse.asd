(defsystem "cl-morse"
  :version "1.0.0"
  :author "em7"
  :license "BSD-3clause"
  :depends-on ("cl-ppcre")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "Library for translating ASCII letters to morse code and back."
  :in-order-to ((test-op (test-op "cl-morse/tests"))))

(defsystem "cl-morse/tests"
  :author "em7"
  :license "BSD-3clause"
  :depends-on ("cl-morse"
               "fiveam")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-morse"
  :perform (test-op (o s)
                    (uiop:symbol-call :fiveam '#:run!
                                      (uiop:find-symbol* '#:all-tests
                                                         '#:cl-morse/tests/main))))
