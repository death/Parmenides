(asdf:defsystem #:frulekit
  :description ""
  :author ""
  :license  ""
  :version "0.0.1"
  :depends-on ("parmenides")
  :serial t
  :pathname "src/frk/"
  :components ((:file "package")
               (:file "fr-messages-eng")
               (:file "fr-messages-esp")
               (:file "build")
               (:file "inter")
               (:file "agenda")
               (:file "trace")))
