;;; -*- mode: Lisp; Syntax: COMMON-LISP; Package: CL-MANGO; Base: 10 -*-

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

           *mango-explain*
           
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
           
           #:couchdb-request

           #:send-json
           
           #:defmango
           #:doc
           #:doc-id
           #:doc-rev
           #:mango-unexpected-http-response))

(in-package #:cl-mango)

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *host* nil)
  (defparameter *port* nil)
  (defparameter *scheme* :http)
  (defparameter *username* nil)
  (defparameter *password* nil)
  (defparameter *explain* nil)
  
  (setf drakma:*text-content-types* (list (cons "application" "json")))
  (setf yason:*parse-json-booleans-as-symbols* t)
  
  (defun symb (a b)
    (intern (format nil "~a-~a" (symbol-name a) (symbol-name b)))))



(defun make-request-uri (req-path)
  (with-output-to-string (sink)
    (puri:render-uri
     (make-instance 'puri:uri :scheme *scheme*
                              :host *host*
                              :port *port*
                              :path req-path) sink)))

(define-condition mango-unexpected-http-response ()
  ((status-code :initform nil :initarg :status-code :reader status-code)
   (status-body :initform nil :initarg :body :reader status-body))
  (:report (lambda (condition stream)
             (format stream "~a ~a" (status-code condition) (status-body condition)))))

(defmacro send-json (&rest body)
  (alexandria:with-gensyms (sink)
    `(with-output-to-string (,sink)
       (yason:encode ,@body ,sink))))

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
                              ,@(when (and (not (null *username*))
                                           (not (null *password*)))
                                  `(:basic-authorization (list *username* *password*)))
                              ,@(when preserve-uri `(:preserve-uri t))
                              :external-format-in :utf8
                              :external-format-out :utf8
                              :connection-timeout 60
                              ,@(when parameters `(:parameters ,parameters))
                              ,@(when content `(:content ,content)))
       (check-type ,status fixnum)
       (if (not (member ,status (list 200 201)))
           (error 'mango-unexpected-http-response
                  :status-code ,status
                  :body ,body)
           (progn
             (when *explain*
               (ignore-errors
                (let ((,warning (gethash "warning" (yason:parse ,body))))
                  (when ,warning (log:info ,warning)))))
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

(defmacro query-view (db view index &key parameters)
  `(couchdb-request (format nil "/~a/_design/~a/_view/~a" ,db ,view ,index)
                    ,@(when parameters `(:parameters ,parameters))))

(defun doc-get (db docid)
  (declare (type string db docid))
  (couchdb-request (format nil "/~a/~a" db docid)))

(defun doc-find (db query)
  (declare (type string db query))
  (couchdb-request (format nil "/~a/_find" db)
                   :method :post
                   :content query))

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


(defmacro make-selector (selector &key (limit 100) fields sort skip)
  (let ((sink (gensym)))
    `(with-output-to-string (,sink)
       (yason:encode (alist-hash-table (list (cons "limit" ,limit)
                                             ,@(when skip
                                                 `((cons "skip" ,skip)))
                                             ,@(when sort
                                                 `((cons "sort" ,sort)))
                                             ,@(when fields
                                                 `((cons "fields" ,fields)))
                                             (cons "selector" (alist-hash-table ,selector))))
                     ,sink))))

(defun %json-to-clos (bundle class &key (doc-name "docs"))
  (check-type bundle string)
  (mapcar (lambda (doc)
            (json-mop:json-to-clos doc class))
          (gethash doc-name (yason:parse bundle))))

(defun mango-get-all (db class)
  (check-type class symbol)
  (mapcar #'(lambda (doc)
              (json-mop:json-to-clos (gethash "doc" doc) class))
          (gethash "rows" (yason:parse (doc-get-all db)))))

(defun mango-find (db class query)
  (check-type query list)
  (%json-to-clos (doc-find db (make-selector query)) class))

(defun mango-update (db object)
  (couchdb-request (format nil "/~a" db)
                   :method :post
                   :content (with-output-to-string (sink)
                              (json-mop:encode object sink))))

(defun allowed-slot-p (class name)
  (declare (optimize (debug 0) (speed 3) (safety 1)))
  (declare (type symbol class)
           (type string name))
  (member name
          (mapcar (lambda (slot)
                    (string-downcase
                     (symbol-name
                      (closer-mop:slot-definition-name slot))))
                  (sb-mop:class-direct-slots (find-class class)))
          :test #'string=))

(defmacro defmango (name database slot-definitions &key (superclass nil))
  (let* ((name-string (format nil "~a" name))
         (name-symbol (intern (symbol-name name)))
         (name-db-name (string-downcase database)))
    `(progn
       (defclass ,name (,superclass) ((id :initarg :id
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
         (mango-find ,name-db-name
                     ',name
                     (list (cons "type" (string-downcase ,name-string)))))

       (defun ,(symb name 'by-id) (id)
         (json-mop:json-to-clos (doc-get ,name-db-name id) ',name-symbol))
       
       (defun ,(symb name 'put) (object)
         (mango-update ,name-db-name object))
       
       (defun ,(symb name 'update) (object)
         (mango-update ,name-db-name object))

       (defmacro ,(symb name 'find-explicit) (query &rest args)
         `(%json-to-clos (doc-find ,',name-db-name (make-selector ,query ,@args))
                         ',',name-symbol))
       
       (defun ,(symb name 'find) (query)
         (let ((query-slots (mapcar #'car query)))
           (alexandria:if-let
               ((bad-name (remove-if #'null
                                     (mapcar (lambda (slot-name)
                                               (allowed-slot-p ',name-symbol slot-name))
                                             query-slots))))
             (mango-find ,name-db-name ',name-symbol
                         (append (list (cons "type" (string-downcase ,name-string))) query))
             (error (format nil "Can't query against a slot that isn't bound to the class: ~a" bad-name)))))
       
       ;; (defun ,(symb name 'delete) (object)
       ;;   (doc-delete ,name-db-name (,(symb name :id) object) (,(symb name :rev) object)))
       
       (defmacro ,(symb name 'create) (&rest args)
         (alexandria:with-gensyms (new-instance result)
           `(let* ((,new-instance (make-instance ',',name-symbol ,@args))
                   (,result (,',(symb name :put) ,new-instance)))
              (gethash "id" (yason:parse ,result))))))))
