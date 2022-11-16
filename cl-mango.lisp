;;; -*- mode: Lisp; Syntax: COMMON-LISP; Package: CL-MANGO -*-

(defpackage #:cl-mango
  (:use #:cl
        #:json-mop)
  (:nicknames "mango" "MANGO")
  (:import-from :alexandria :hash-table-keys
                :alist-hash-table :when-let)
  (:export *host*
           *port*
           *scheme*
           *username*
           *password*
           *explain*

           #:compact

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

           #:unexpected-http-response
           #:defmango))

(in-package #:cl-mango)

(eval-when (:compile-toplevel :load-toplevel)
  (defvar *host* nil)
  (defvar *port* nil)
  (defvar *scheme* :http)
  (defvar *username* nil)
  (defvar *password* nil)
  (defvar *explain* nil)

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

(defmethod print-object ((condition unexpected-http-response) stream)
  (print-unreadable-object (condition stream)
    (with-slots (status-code status-body)
        condition
      (format stream "~a: ~a" status-code status-body))))

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
                              :connection-timeout 120
                              ,@(when parameters `(:parameters ,parameters))
                              ,@(when content `(:content ,content)))
       (check-type ,status fixnum)
       (if (not (member ,status (list 200 201 202)))
         (error 'unexpected-http-response
                :status-code ,status
                :body ,body)
         (progn ,body)))))

(defun doc-batch-put (db bundle)
  (declare (type string db bundle))
  (couchdb-request (format nil "/~a?batch=ok" db)
                   :method :post
                   :content bundle))

(defun doc-put (db bundle)
  (declare (type string db)
           (list bundle))
  (couchdb-request (format nil "/~a" db)
                   :method :post
                   :content (jsown:to-json bundle)))

(defun doc-get (db docid)
  (declare (type string db docid))
  (jsown:parse (couchdb-request (format nil "/~a/~a" db docid))))


(defun compact (database)
  (couchdb-request (format nil "/~a/_compact" database)
                   :method :post
                   :content "{}"))

(defun make-selector (selector &key (limit 100) fields sort skip
                       stale use-index r bookmark update stable execution-stats)
  (jsown:to-json `(:obj ("limit" . ,limit)
                        ,@(when skip
                            `(("skip" . ,skip)))
                        ,@(when sort
                            `(("sort" . ,sort)))
                        ,@(when fields
                            `(("fields" . ,fields)))
                        ,@(when execution-stats
                            `(("execution_stats" . "true")))
                        ,@(when stable
                            `(("stable" . "true")))
                        ,@(when stale
                            `(("stale" . "true")))
                        ,@(when update
                            `(("update" . "true")))
                        ,@(when bookmark
                            `(("bookmark" . ,bookmark)))
                        ,@(when r
                            `(("r" . ,r)))
                        ,@(when use-index
                            `(("use_index" . ,use-index)))
                        ("selector" . ,selector))))

(defun doc-find (db query)
  (declare (type string db query))
  (jsown:val (jsown:parse (couchdb-request (format nil "/~a/_find" db)
                                           :method :post
                                           :content query))
             "docs"))

(defmacro couch-query (database selector &rest args)
  `(doc-find ,database (make-selector ,selector ,@args)))

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



;; This was entirely a bad idea.  Leaving it here because sometimes
;; bad ideas become good ideas, plus it's a good example of how to do
;; some things.

;; (defun symb (a b)
;;   (intern (format nil "~a-~a" (symbol-name a) (symbol-name b))))

;; (defun %json-to-clos (bundle class &key (doc-name "docs"))
;;   (check-type bundle string)
;;   (mapcar (lambda (doc)
;;             (cl-ivy:make-to-json-instance )
;;             (json-mop:json-to-clos doc class))
;;           (jsown:val (jsown:parse bundle) doc-name)))

;; (defun mango-get-all (db class)
;;   (check-type class symbol)
;;   (mapcar #'(lambda (doc)
;;               (json-mop:json-to-clos (gethash "doc" doc) class))
;;           (jsown:val (jsown:parse (doc-get-all db)) "rows")))

;; (defun mango-find (db class query)
;;   (check-type query list)
;;   (%json-to-clos (doc-find db (make-selector query)) class))

;; (defun mango-update (db object)
;;   (couchdb-request (format nil "/~a" db)
;;                    :method :post
;;                    :content (with-output-to-string (sink)
;;                               (json-mop:encode object sink))))

;; (defun allowed-slot-p (class name)
;;   (declare (type symbol class)
;;            (type string name))
;;   (member name
;;           (mapcar (lambda (slot)
;;                     (string-downcase
;;                      (symbol-name
;;                       (closer-mop:slot-definition-name slot))))
;;                   #+sbcl (sb-mop:class-direct-slots (find-class class))
;;                   #+lispworks (harlequin-common-lisp:class-direct-slots (find-class class))
;;                   #+ccl (ccl:class-direct-slots (find-class class)))
;;           :test #'string=))

;; (defmacro defmango (name database slot-definitions)
;;   (let* ((name-string (format nil "~a" name))
;;          (name-symbol (intern (symbol-name name)))
;;          (name-db-name (string-downcase database)))
;;     `(progn
;;        (defclass ,name () ((-id :initarg :-id
;;                                 :json-type :string
;;                                 :json-key "_id"
;;                                 :accessor ,(symb name :-id))
;;                            (-rev :initarg :-rev
;;                                  :json-type :string
;;                                  :json-key "_rev"
;;                                  :accessor ,(symb name :rev))
;;                            (type :initarg :type
;;                                  :json-type :string
;;                                  :json-key "type"
;;                                  :initform (string-downcase ,name-string))
;;                            ,@slot-definitions)
;;          (:metaclass json-serializable-class))


;;        (defun ,(symb name 'get-all) ()
;;          (mango-find ,name-db-name
;;                      ',name
;;                      (list (cons "type" (string-downcase ,name-string)))))

;;        (defun ,(symb name 'get) (id)
;;          (json-mop:json-to-clos (doc-get ,name-db-name id) ',name-symbol))

;;        (defun ,(symb name 'put) (object)
;;          (mango-update ,name-db-name object))

;;        (defun ,(symb name 'update) (object)
;;          (mango-update ,name-db-name object))

;;        (defmacro ,(symb name 'find) (query &rest query-args)
;;          `(let ((query-slots (mapcar #'car ,query)))
;;             (if (remove-if #'null (mapcar #'(lambda (slot-name)
;;                                               (allowed-slot-p ',',name-symbol
;;                                                               slot-name))
;;                                           query-slots))

;;                 (let* ((new-query (append (list (cons "type"
;;                                                       (string-downcase
;;                                                        ,',name-string)))
;;                                           ,query))
;;                        (selector (make-selector new-query
;;                                                 ,@(when query-args
;;                                                     `(,@query-args)))))
;;                   (mapcar #'(lambda (doc)
;;                               (json-mop:json-to-clos doc ',',name-symbol))
;;                           (jsown:val (jsown:parse
;;                                       (doc-find ,',name-db-name
;;                                                 selector))
;;                                      "docs"))))))

;;        (defun ,(symb name 'delete) (object)
;;          (doc-delete ,name-db-name (,(symb name :-id) object) (,(symb name :rev) object)))

;;        (defun ,(symb name 'from-json) (string)
;;          (json-mop:json-to-clos string ',name-symbol))

;;        (defun ,(symb name 'to-json) (object)
;;          (with-output-to-string (sink)
;;            (json-mop:encode object sink)))

;;        (defmacro ,(symb name 'create) (&rest args)
;;          (alexandria:with-gensyms (new-instance result)
;;            `(let* ((,new-instance (make-instance ',',name-symbol ,@args))
;;                    (,result (mango-update ',',name-db-name ,new-instance)))
;;               (jsown:val (jsown:parse ,result) "id")))))))
