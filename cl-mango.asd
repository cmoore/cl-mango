;;;; cl-mango.asd

(asdf:defsystem #:cl-mango
  :description "Query CouchDB 2.0 databases via the Mango engine."
  :author "Clint Moore <clint@ivy.io>"
  :license "BSD"
  :serial t
  :depends-on (#:drakma
               #:yason
               #:json-mop)
  :components ((:file "package")
               (:file "cl-mango")))

