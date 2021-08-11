(defpackage :mita.clack.util
  (:export :ensure-integer
           :ensure-uuid-short
           :html-response
           :json-response
           :bad-request
           :server-error
           :q
           :node
           :nodes
           :node->middleware)
  (:use :cl))
(in-package :mita.clack.util)

(define-condition bad-request () ())

(define-condition server-error () ())

(defun ensure-integer (obj &optional default-value)
  (cond ((null obj)
         (if (typep default-value 'integer)
             default-value
             (error 'server-error)))
        ((stringp obj)
         (handler-case (parse-integer obj)
           (error ()
             (error 'bad-request))))
        (t
         (error 'bad-request))))

(defun ensure-uuid-short (obj)
  (if (stringp obj)
      (handler-case (mita.id:parse-short obj)
        (error ()
          (error 'bad-request)))
      (error 'bad-request)))



(defun html-response (body-string &key (status-code 200))
  `(,status-code (:content-type "text/html")
                 (,body-string)))

(defun json-response (value &key (status-code 200) (success t))
  `(,status-code (:content-type "application/json")
                 (,(jsown:to-json
                    (jsown:new-js
                      ("success" (or success :f))
                      ("value" value))))))

(defun q (req name)
  (let ((params (lack.request:request-parameters req)))
    (cdr (assoc name params :test #'string=))))

(defstruct node
  path
  method-fn-list
  children)

(defmacro node ((path &rest clauses))
  (let ((children nil)
        (method-fn-list nil))
    (dolist (clause clauses)
      (etypecase (car clause)
        (string
         (push `(node (,@clause)) children))
        (keyword
         (destructuring-bind (method fn) clause
           (push `(list ,method
                        (lambda (params)
                          (lambda (req)
                            (,fn params req))))
                 method-fn-list)))))
    `(make-node :path ,path
                :method-fn-list (list ,@method-fn-list)
                :children (list ,@children))))

(defun connect-node (mapper node parent-path)
  (let ((path (concatenate 'string
                           parent-path
                           (node-path node))))
    (loop for (method fn) in (node-method-fn-list node) do
      (myway:connect mapper path fn :method method)
      (format *standard-output* "Connected: ~10A ~A~%" method path))
    (loop for child in (node-children node) do
      (connect-node mapper child path))))

(defun nodes (nodes)
  (make-node :path "" :children nodes))

(defun node->middleware (node)
  (let ((mapper (myway:make-mapper)))
    (connect-node mapper node "")
    (lambda (app)
      (lambda (env)
        (or (let ((request (lack.request:make-request env)))
              (multiple-value-bind (handler foundp)
                  (let ((method (lack.request:request-method request))
                        (path-info (lack.request:request-path-info request)))
                    (myway:dispatch mapper path-info :method method))
                (when foundp
                  (funcall handler request))))
            (funcall app env))))))
