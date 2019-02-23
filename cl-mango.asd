
(asdf:defsystem #:cl-mango
  :description "A minimalist CouchDB 2.x database client."
  :author "Clint Moore <clint@ivy.io>"
  :license "BSD3"
  :serial t
  :depends-on (#:drakma
               #:yason
               #:json-mop
               #:log4cl)
  
  :components ((:file "cl-mango")))
