
(defpackage #:cl-mango
  (:use #:cl)
  (:import-from :alexandria :hash-table-keys
                :alist-hash-table))

(in-package #:cl-mango)


(defparameter *mango-host* nil)
(defparameter *mango-port* nil)
(defparameter *mango-scheme* "http")
(defparameter *mango-username* nil)
(defparameter *mango-password* nil)

(defparameter *mango-method* nil)

(defmacro make-request (path &key (parameters nil) (content nil))
  (let ((t-result (gensym)))
    `(labels ((make-request-uri (req-path)
                (with-output-to-string (sink)
                  (puri:render-uri (make-instance 'puri:uri :scheme *mango-scheme*
                                                            :host *mango-host*
                                                            :port *mango-port*
                                                            :path req-path) sink))))
       
       (let ((,t-result (drakma:http-request (make-request-uri ,path)
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
         
         (values (yason:parse ,t-result) ,t-result)))))


(defun test ()
  (let ((*mango-host* "127.0.0.1")
        (*mango-port* "5984")
        (*mango-scheme* :http)
        (*mango-username* "tools")
        (*mango-password* "grapevine")
        (*mango-method* :post)
        (drakma:*text-content-types* (list (cons "application" "json"))))
    (let ((json-to-send (with-output-to-string (sink)
                          (yason:encode (let ((nh (make-hash-table)))
                                          (setf (gethash "selector" nh) (make-hash-table))
                                          nh) sink))))
      (make-request "/scanner/_find" :content json-to-send)))
  )

