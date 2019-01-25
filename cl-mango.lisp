;;; -*- mode: Lisp; Syntax: COMMON-LISP; Package: CL-MANGO -*-

(defpackage #:cl-mango
  (:use #:cl
        #:json-mop
        #:log4cl)
  (:nicknames "mango")
  (:import-from :alexandria :hash-table-keys
                :alist-hash-table)
  (:export *host*
           *port*
           *scheme*
           *username*
           *password*
           *explain*
           
           #:doc-put
           #:doc-batch-put
           
           #:doc-get
           #:doc-find
           #:doc-get-all
           #:doc-delete

           #:bulk-docs
           
           #:query-view
           
           #:make-selector
           #:couch-query
           
           #:unexpected-http-response))

(in-package #:cl-mango)

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *host* nil)
  (defparameter *port* nil)
  (defparameter *scheme* :http)
  (defparameter *username* nil)
  (defparameter *password* nil)
  
  ;; If there is a warning about the lack of a usable index for
  ;; a mango query, send that warning to the console using log4cl
  (defparameter *explain* nil)

  (setf drakma:*text-content-types* (list (cons "application" "json"))))

(defun make-request-uri (req-path)
  (with-output-to-string (sink)
    (puri:render-uri
     (make-instance 'puri:uri :scheme *scheme*
                              :host *host*
                              :port *port*
                              :path req-path) sink)))

(define-condition unexpected-http-response ()
  ((status-code :initform nil
                :initarg :status-code
                :reader status-code)
   (status-body :initform nil
                :initarg :body
                :reader status-body))
  (:report (lambda (condition stream)
             (format stream "~a ~a" (status-code condition) (status-body condition)))))

(defmacro couchdb-request (path &key
                                (parameters nil)
                                (content nil)
                                (method :get)
                                (content-type "application/json")
                                (accept "application/json")
                                (preserve-uri))
  (alexandria:with-gensyms (body status warning)
    `(multiple-value-bind (,body ,status)
         (drakma:http-request (make-request-uri ,path)
                              :accept ,accept
                              :content-type ,content-type
                              :method ,method
                              :basic-authorization (list *username* *password*)
                              ,@(when preserve-uri `(:preserve-uri t))
                              :external-format-in :utf8
                              :external-format-out :utf8
                              :connection-timeout 60
                              ,@(when parameters `(:parameters ,parameters))
                              ,@(when content `(:content ,content)))
       (check-type ,status fixnum)
       (if (not (member ,status (list 200 201)))
         (error 'unexpected-http-response
                :status-code ,status
                :body ,body)
         (progn
           (when *explain*
             (let ((,warning (gethash "warning" (yason:parse ,body))))
               (when ,warning
                 ,(if (find-package :log4cl)
                    `(progn
                       (log:info ,warning)
                       (log:info ,parameters)
                       (log:info ,content))
                    `(progn
                       (format t ,warning)
                       (format t ,parameters)
                       (format t ,content))))))
           ,body)))))

(defun doc-batch-put (db bundle)
  (declare (type string db bundle))
  (couchdb-request (format nil "/~a?batch=ok" db)
                   :method :post
                   :content bundle))

(defun doc-put (db bundle)
  (declare (type string db bundle))
  (couchdb-request (format nil "/~a" db)
                   :method :post
                   :content bundle))

(defun doc-get (db docid)
  (declare (type string db docid))
  (couchdb-request (format nil "/~a/~a" db docid)))

(defmacro make-selector (selector &key (limit 100) fields sort skip stale use-index r bookmark update stable execution-stats)
  (let ((sink (gensym)))
    `(with-output-to-string (,sink)
       (yason:encode (alist-hash-table (list (cons "limit" ,limit)
                                             ,@(when skip
                                                 `((cons "skip" ,skip)))
                                             ,@(when sort
                                                 `((cons "sort" ,sort)))
                                             ,@(when fields
                                                 `((cons "fields" ,fields)))
                                             ,@(when execution-stats
                                                 `((cons "execution_stats" "true")))
                                             ,@(when stable
                                                 `(cons "stable" "true"))
                                             ,@(when stale
                                                 `(cons "stale" "true"))
                                             ,@(when update
                                                 `(cons "update" "true"))
                                             ,@(when bookmark
                                                 `(cons "bookmark" ,bookmark))
                                             ,@(when r
                                                 `(cons "r" ,r))
                                             ,@(when use-index
                                                 `(cons "use_index" ,use-index))
                                             (cons "selector" (alist-hash-table ,selector))))
                     ,sink))))

(defun doc-find (db query)
  (declare (type string db query))
  (couchdb-request (format nil "/~a/_find" db)
                   :method :post
                   :content query))

(defmacro couch-query (selector &rest args)
  `(doc-find "reddit" (make-selector ,selector ,@args)))

(defun doc-get-all (db &key (all-docs nil))
  (let ((args (if all-docs
                  (format nil "/~a/_all_docs?include_docs=true" db)
                  (format nil "/~a/_all_docs" db))))
    (couchdb-request args)))

(defun doc-delete (db docid revision)
  (couchdb-request (format nil "/~a/~a?rev=~a" db docid revision)
                   :method :delete))

(defun bulk-docs (db bundle)
  (couchdb-request (format nil "/~a/_bulk_docs" db)
                   :method :post
                   :content bundle))


(defmacro query-view (db view index parameters)
  `(couchdb-request (format nil "/~a/_design/~a/_view/~a" ,db ,view ,index)
                    ,@(when parameters `(:parameters ,parameters))
                    :method :get))
