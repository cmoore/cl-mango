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
  (defparameter *mango-host* nil)
  (defparameter *mango-port* nil)
  (defparameter *mango-scheme* :http)
  (defparameter *mango-username* nil)
  (defparameter *mango-password* nil)

  (setf drakma:*text-content-types* (list (cons "application" "json")))

  (defun symb (a b)
    (intern (format nil "~a-~a" (symbol-name a) (symbol-name b)))))

(defmacro make-couchdb-request (path &key (parameters nil) (content nil) (method :get))
  `(labels ((make-request-uri (req-path)
              (with-output-to-string (sink)
                (puri:render-uri (make-instance 'puri:uri :scheme *mango-scheme*
                                                          :host *mango-host*
                                                          :port *mango-port*
                                                          :path req-path) sink))))
     
     (let ((response (drakma:http-request
                      (make-request-uri ,path)
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
                      ,@(when parameters `(:parameters ,parameters))
                      ,@(when content `(:content ,content)))))
       (cond ((equal (type-of response) '(simple-vector 2)) (flexi-streams:octets-to-string response))
             (t response)))))

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

(defun make-selector (parameters)
  (check-type parameters list)
  (with-output-to-string (sink)
    (yason:encode (alist-hash-table
                   (list (cons "limit" 10000)
                         (cons "selector"
                                  (alist-hash-table parameters)))) sink)))

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
                            (when (getf (cdr spec) :export)
                              (let ((name (getf (cdr spec) :accessor)))
                                (list name))))
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

       ;; Export symbols for all accessors marked as 'export'
       ,@(mapcar (lambda (name) `(export ',name))
                 exports)

       (defun ,(symb name 'get-all) ()
         (mapcar #'(lambda (doc)
                     (json-mop:json-to-clos (gethash "doc" doc) ',name-symbol))
                 (gethash "rows" (yason:parse (doc-get-all ,name-db-name)))))

       (defun ,(symb name 'put) (object)
         (mango-update ,name-db-name object))
       
       (defun ,(symb name 'update) (object)
         (mango-update ,name-db-name object))

       (defun ,(symb name 'find) (query)
         (mango-find ,name-db-name ',name-symbol query))

       (defun ,(symb name 'delete) (object)
         (doc-delete ,name-db-name (,(symb name :id) object) (,(symb name :rev) object)))
       
       (defmacro ,(symb name 'create) (&rest args)
         (let ((new-name (gensym)))
           `(let ((,new-name (make-instance ',',name-symbol ,@args)))
              (,',(symb name :put) ,new-name)))))))

