(asdf:defsystem :bobbin
  :description "Simple (word) wrapping utilities for strings."
  :author "Steve Losh <steve@stevelosh.com>"
  :homepage "https://docs.stevelosh.com/bobbin/"

  :license "MIT"
  :version "1.0.1"

  :depends-on (:split-sequence)

  :in-order-to ((asdf:test-op (asdf:test-op :bobbin/test)))

  :serial t
  :components ((:file "package")
               (:module "src" :serial t
                :components ((:file "main")))))


(asdf:defsystem :bobbin/test
  :description "Test suite for bobbin"

  :author "Steve Losh <steve@stevelosh.com>"
  :license "MIT"

  :depends-on (:bobbin :1am)

  :serial t
  :components ((:file "package.test")
               (:module "test"
                :serial t
                :components ((:file "tests"))))

  :perform (asdf:test-op (op system)
             (funcall (read-from-string "bobbin.test:run-tests"))))
