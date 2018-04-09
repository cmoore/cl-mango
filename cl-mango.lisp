;; -*- mode: Lisp; Syntax: COMMON-LISP; Base: 10; eval: (hs-hide-all) -*-

(defpackage #:cl-mango
  (:use #:cl
        #:json-mop
        #:log4cl)
  (:nicknames "mango")
  (:import-from :alexandria :hash-table-keys
                :alist-hash-table)
  (:export *mango-host*
           *mango-port*
           *mango-scheme*
           *mango-username*
           *mango-password*
           
           #:doc-put
           #:doc-batch-put
           
           #:doc-get
           #:doc-find
           #:doc-get-all
           #:doc-delete

           #:bulk-docs
           
           #:query-view
           
           #:mango-get-all
           #:mango-find
           #:mango-update
           #:make-selector
           
           #:make-couchdb-request

           #:send-json
           
           #:defmango

           #:mango-unexpected-http-response))

(in-package #:cl-mango)

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *mango-host* nil)
  (defparameter *mango-port* nil)
  (defparameter *mango-scheme* :http)
  (defparameter *mango-username* nil)
  (defparameter *mango-password* nil)

  (setf drakma:*text-content-types* (list (cons "application" "json")))

  (defun symb (a b)
    (intern (format nil "~a-~a" (symbol-name a) (symbol-name b)))))

(defun make-request-uri (req-path)
    (with-output-to-string (sink)
      (puri:render-uri (make-instance 'puri:uri :scheme *mango-scheme*
                                                :host *mango-host*
                                                :port *mango-port*
                                                :path req-path) sink)))

(define-condition mango-unexpected-http-response ()
  ((status-code :initform nil :initarg :status-code :reader status-code)
   (status-body :initform nil :initarg :body :reader status-body))
  (:report (lambda (condition stream) (format stream "~a ~a" (status-code condition) (status-body condition)))))

(defmacro send-json (&rest body)
  (alexandria:with-gensyms (sink)
    `(with-output-to-string (,sink)
       (yason:encode ,@body ,sink))))

(defmacro make-couchdb-request (path &key
                                       (parameters nil)
                                       (content nil)
                                       (method :get)
                                       (content-type "application/json")
                                       (accept "application/json")
                                       (preserve-uri))
  `(multiple-value-bind (body status)
       (drakma:http-request (make-request-uri ,path)
                            :accept ,accept
                            :content-type ,content-type
                            :method ,method
                            ,@(when (and (not (null *mango-username*))
                                         (not (null *mango-password*)))
                                `(:basic-authorization (list *mango-username* *mango-password*)))
                            ,@(when preserve-uri `(:preserve-uri t))
                            :external-format-in :utf8
                            :external-format-out :utf8
                            :connection-timeout 60
                            ,@(when parameters `(:parameters ,parameters))
                            ,@(when content `(:content ,content)))
     (if (not (member status (list 200 201)))
         (error 'mango-unexpected-http-response
                :status-code status
                :body body)
         body)))

(defun doc-batch-put (db bundle)
  (make-couchdb-request (format nil "/~a?batch=ok" db)
                        :method :post
                        :content bundle))

(defun doc-put (db bundle)
  (make-couchdb-request (format nil "/~a" db)
                        :method :post
                        :content bundle))

(defmacro query-view (db view index &key parameters)
  `(make-couchdb-request (format nil "/~a/_design/~a/_view/~a" ,db ,view ,index)
                         ,@(when parameters `(:parameters ,parameters))))

(defun doc-get (db docid)
  (make-couchdb-request (format nil "/~a/~a" db docid)))

(defun doc-find (db query)
  (make-couchdb-request (format nil "/~a/_find" db)
                        :method :post
                        :content query))

(defun doc-get-all (db)
  (make-couchdb-request (format nil "/~a/_all_docs?include_docs=true" db)))

(defun doc-delete (db docid revision)
  (make-couchdb-request (format nil "/~a/~a?rev=~a" db docid revision)
                        :method :delete))

(defun bulk-docs (db bundle)
  (make-couchdb-request (format nil "/~a/_bulk_docs" db)
                        :method :post
                        :content bundle))

(defmacro make-selector (selector &key (limit 100) fields sort skip)
  `(with-output-to-string (sink)
     (yason:encode (alist-hash-table (list (cons "limit" ,limit)
                                           ,@(when skip
                                               `((cons "skip" ,skip)))
                                           ,@(when sort
                                               `((cons "sort" ,sort)))
                                           ,@(when fields
                                               `((cons "fields" ,fields)))
                                           (cons "selector" (alist-hash-table ,selector))))
                   sink)))

(defun class-ify-couch-response (bundle class &key (doc-name "docs"))
  (check-type bundle string)
  (mapcar #'(lambda (doc)
              (json-mop:json-to-clos doc class))
          (gethash doc-name (yason:parse bundle))))

(defun mango-get-all (db class)
  (check-type class symbol)
  (mapcar #'(lambda (doc)
              (json-mop:json-to-clos (gethash "doc" doc) class))
          (gethash "rows" (yason:parse (doc-get-all db)))))

(defun mango-find (db class query)
  (check-type query list)
  (class-ify-couch-response
   (doc-find db (make-selector query)) class))

(defun mango-update (db object)
  (make-couchdb-request (format nil "/~a" db)
                        :method :post
                        :content (with-output-to-string (sink)
                                   (json-mop:encode object sink))))

(defmacro defmango (name database slot-definitions)
  (let* ((name-string (format nil "~a" name))
         (name-symbol (intern (symbol-name name)))
         (name-db-name (string-downcase database)))
    `(progn
       (defclass ,name () ((id :initarg :id
                               :json-type :string
                               :json-key "_id"
                               :accessor ,(symb name :id))
                           (rev :initarg :rev
                                :json-type :string
                                :json-key "_rev"
                                :accessor ,(symb name :rev))
                           (type :initarg :type
                                 :json-type :string
                                 :json-key "type"
                                 :initform (string-downcase ,name-string))
                           ,@slot-definitions)
         (:metaclass json-serializable-class))

       (defun ,(symb name 'get-all) ()
         (mapcar #'(lambda (doc)
                     (json-mop:json-to-clos doc ',name-symbol))
                 (gethash "docs" (yason:parse (doc-find ,name-db-name (make-selector (list (cons "type" ,name-string))))))))
       
       (defun ,(symb name 'put) (object)
         (mango-update ,name-db-name object))
       
       (defun ,(symb name 'update) (object)
         (mango-update ,name-db-name object))
       
       (defmacro ,(symb name 'find-explicit) (query &rest args)
         `(class-ify-couch-response
           (doc-find ,(string-downcase ,name-string) (make-selector ,query ,@args))
           ',',name-symbol))
       
       (defun ,(symb name 'find) (query)
         (mango-find ,name-db-name ',name-symbol query))
       
       (defun ,(symb name 'delete) (object)
         (doc-delete ,name-db-name (,(symb name :id) object) (,(symb name :rev) object)))


       
       (defmacro ,(symb name 'create) (&rest args)
         (let ((new-name (gensym)))
           `(let ((,new-name (make-instance ',',name-symbol ,@args)))
              (,',(symb name :put) ,new-name))))

       ;; (defmacro ,(symb name 'create) (&rest args)
       ;;   `(make-instance ',',name-symbol ,@args))
       )))
