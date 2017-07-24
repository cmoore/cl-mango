;; -*- mode: Lisp; Syntax: COMMON-LISP; Base: 10; eval: (hs-hide-all) -*-

(defpackage #:cl-mango
  (:use #:cl)
  (:nicknames "mango")
  (:import-from :alexandria :hash-table-keys
                :alist-hash-table)
  (:export *mango-host*
           *mango-port*
           *mango-scheme*
           *mango-username*
           *mango-password*
           *mango-method*
           
           #:doc-put
           #:doc-get
           #:doc-find
           #:doc-get-all))

(in-package #:cl-mango)

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *mango-host* nil)
  (defparameter *mango-port* nil)
  (defparameter *mango-scheme* :http)
  (defparameter *mango-username* nil)
  (defparameter *mango-password* nil)

  (defparameter *mango-method* nil)

  (drakma:*text-content-types* (list (cons "application" "json"))))

(defmacro make-couchdb-request (path &key (parameters nil) (content nil))
  `(labels ((make-request-uri (req-path)
              (with-output-to-string (sink)
                (puri:render-uri (make-instance 'puri:uri :scheme *mango-scheme*
                                                          :host *mango-host*
                                                          :port *mango-port*
                                                          :path req-path) sink))))
     
     (drakma:http-request (make-request-uri ,path)
                          :additional-headers (list
                                               (cons "Accept" "application/json"))
                          :content-type "application/json"
                          :method *mango-method*
                          :basic-authorization (list "tools" "grapevine")
                          :preserve-uri t
                          :external-format-in :utf8
                          :external-format-out :utf8
                          ,@(when parameters `(:parameters ,parameters))
                          ,@(when content `(:content ,content)))))

(defun doc-put (db bundle)
  (let ((*mango-method* :post))
    (make-couchdb-request (format nil "/~a" db)
                          :content bundle)))

(defun query-view (db view index)
  (let ((*mango-method* :GET))
    (make-couchdb-request (format nil "/~a/_design/~a/_view/~a" db view index))))

(defun doc-get (db docid)
  (let ((*mango-method* :get))
    (make-couchdb-request (format nil "/~a/~a" db docid))))

(defun doc-find (db query)
  (let ((*mango-method* :post))
    (make-couchdb-request (format nil "/~a/_find" db)
                          :content query)))

(defun doc-get-all (db)
  (let ((*mango-method* :get))
    (make-couchdb-request (format nil "/~a/_all_docs?include_docs=true" db))))
