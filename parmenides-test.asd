(asdf:defsystem #:parmenides-test
  :description "Tests for Parmenides"
  :license "MIT"
  :perform (asdf:test-op (op c) (uiop:symbol-call :parmenides-test :test-all))
  :pathname "tests/"
  :depends-on (#:parmenides)
  :components
  ((:file "package")
   (:file "prtest")))
