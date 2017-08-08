;; -*- mode: Lisp; Syntax: COMMON-LISP; Base: 10; eval: (hs-hide-all) -*-

(defpackage #:cl-mango
  (:use #:cl
        #:json-mop)
  (:nicknames "mango")
  (:import-from :alexandria :hash-table-keys
                :alist-hash-table)
  (:export *mango-host*
           *mango-port*
           *mango-scheme*
           *mango-username*
           *mango-password*
           
           #:doc-put
           #:doc-get
           #:doc-find
           #:doc-get-all

           #:mango-get-all
           #:mango-find
           #:mango-update
           #:make-selector

           #:defmango))

(in-package #:cl-mango)

(eval-when (:compile-toplevel :load-toplevel)

  (setf drakma:*text-content-types* (list (cons "application" "json")))

  (defun symb (a b)
    (intern (format nil "~a-~a" (symbol-name a) (symbol-name b)))))

(defparameter *mango-host* nil)
(defparameter *mango-port* nil)
(defparameter *mango-scheme* :http)
(defparameter *mango-username* nil)
(defparameter *mango-password* nil)

(defun make-request-uri (req-path)
    (with-output-to-string (sink)
      (puri:render-uri (make-instance 'puri:uri :scheme *mango-scheme*
                                                :host *mango-host*
                                                :port *mango-port*
                                                :path req-path) sink)))

(define-condition mango-unexpected-http-response ()
  ((status-code :initform nil :initarg :status-code)))

(defmacro make-couchdb-request (path &key (parameters nil) (content nil) (method :get))
  `(multiple-value-bind (body status)
       (drakma:http-request (make-request-uri ,path)
                            :additional-headers (list
                                                 (cons "Accept" "application/json"))
                            :content-type "application/json"
                            :method ,method
                            ,@(when (and (not (null *mango-username*))
                                         (not (null *mango-password*)))
                                `(:basic-authorization (list *mango-username* *mango-password*)))
                            :preserve-uri t
                            :external-format-in :utf8
                            :external-format-out :utf8
                            :connection-timeout 60
                            ,@(when parameters `(:parameters ,parameters))
                            ,@(when content `(:content ,content)))
     (if (not (member status (list 200 201)))
         (error 'mango-unexpected-http-response
                :status-code status)
         
         body
         ;; (cond ((or (equal (type-of response) '(simple-vector 2))
         ;;            (equal (type-of response) '(simple-vector 8))) (flexi-streams:octets-to-string response))
         ;;       (t response))
         )))



(defun doc-put (db bundle)
  (make-couchdb-request (format nil "/~a" db)
                        :method :post
                        :content bundle))

(defun query-view (db view index)
  (make-couchdb-request (format nil "/~a/_design/~a/_view/~a" db view index)))

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

(defmacro make-selector (selector &key (limit 100) fields sort skip)
  (let ((the-request (gensym)))
    `(let ((,the-request (with-output-to-string (sink)
                           (yason:encode (alist-hash-table (list (cons "limit" ,limit)
                                                                 ,@(when skip
                                                                     `((cons "skip" ,skip)))
                                                                 ,@(when sort
                                                                     `((cons "sort" ,sort)))
                                                                 ,@(when fields
                                                                     `((cons "fields" ,fields)))
                                                                 (cons "selector" (alist-hash-table ,selector))))
                                         sink))))
       (log:info ,the-request)
       ,the-request)))


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

(defmacro defmango (name slot-definitions)
  (let* ((exports (mapcan (lambda (spec)
                            (let ((name (getf (cdr spec) :accessor)))
                              (list name)))
                          slot-definitions))
         (name-string (format nil "~a" name))
         (name-symbol (intern (symbol-name name)))
         (name-db-name (string-downcase name-string)))
    `(progn
       (defclass ,name () ((id :initarg :id
                               :json-type :string
                               :json-key "_id"
                               :accessor ,(symb name :id))
                           (rev :initarg :rev
                                :json-type :string
                                :json-key "_rev"
                                :accessor ,(symb name :rev))
                           ,@slot-definitions)
         (:metaclass json-serializable-class))

       (export ',(symb name 'id))
       (export ',(symb name 'rev))
       
       ,@(mapcar (lambda (name) `(export ',name))
                 exports)

       (defun ,(symb name 'get-all) ()
         (mapcar #'(lambda (doc)
                     (json-mop:json-to-clos (gethash "doc" doc) ',name-symbol))
                 (gethash "rows" (yason:parse (doc-get-all ,name-db-name)))))
       (export ',(symb name 'get-all))
       
       (defun ,(symb name 'put) (object)
         (mango-update ,name-db-name object))
       (export ',(symb name 'put))
       
       (defun ,(symb name 'update) (object)
         (mango-update ,name-db-name object))
       (export ',(symb name 'update))
       
       (defmacro ,(symb name 'find-explicit) (query &rest args)
         `(class-ify-couch-response
           (doc-find ,(string-downcase ,name-string) (make-selector ,query ,@args))
           ',',name-symbol))
       (export ',(symb name 'find-explicit))
       
       (defun ,(symb name 'find) (query)
         (mango-find ,name-db-name ',name-symbol query))
       (export ',(symb name 'find))
       
       (defun ,(symb name 'delete) (object)
         (doc-delete ,name-db-name (,(symb name :id) object) (,(symb name :rev) object)))
       (export ',(symb name 'delete))

       (defmacro ,(symb name 'create) (&rest args)
         (let ((new-name (gensym)))
           `(let ((,new-name (make-instance ',',name-symbol ,@args)))
              (,',(symb name :put) ,new-name))))
       (export ',(symb name 'create))
       
       ,@ (mapcar (lambda (name)
                    `(export ',name))
                  exports))))
