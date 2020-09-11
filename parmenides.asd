(asdf:defsystem #:parmenides
  :description ""
  :author ""
  :license  ""
  :version "0.0.1"
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "parmenides")
               (:file "pa-messages-eng")
               (:file "pa-messages-esp"))
  :in-order-to ((asdf:test-op (asdf:test-op #:parmenides-test))))
