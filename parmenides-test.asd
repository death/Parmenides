(asdf:defsystem #:parmenides-test
  :description "Tests for Parmenides"
  :license "MIT"
  :depends-on (#:parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parmenides-test :test-all))
  :pathname "src/"
  :components
  ((:file "prtest")))
