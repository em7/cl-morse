(defsystem "cl-morse"
  :version "0.1.0"
  :author "em7"
  :license "BSD-3clause"
  :depends-on ("cl-ppcre")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-morse/tests"))))

(defsystem "cl-morse/tests"
  :author "em7"
  :license "BSD-3clause"
  :depends-on ("cl-morse"
               "dissect"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-morse"
  :perform (test-op (op c) (symbol-call :rove :run c)))
