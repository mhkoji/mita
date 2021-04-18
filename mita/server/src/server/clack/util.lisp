(defpackage :mita.server.clack.util
  (:export :ensure-integer
           :ensure-uuid-short
           :html-response
           :json-response
           :bad-request
           :server-error
           :q
           :connect-all
           :mapper->middleware)
  (:use :cl))
(in-package :mita.server.clack.util)

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

(defmacro connect-all (mapper arg-list)
  `(progn
     ,@(mapcar (lambda (arg)
                 (destructuring-bind (endpoint fn) arg
                   (destructuring-bind (url &rest rest)
                       (alexandria:ensure-list endpoint)
                     `(myway:connect ,mapper ,url
                                     (lambda (params)
                                       (lambda (req)
                                         (,fn params req)))
                                     ,@rest))))
               arg-list)))

(defun mapper->middleware (mapper)
  (lambda (app)
    (lambda (env)
      (or (let ((request (lack.request:make-request env)))
            (multiple-value-bind (handler foundp)
                (let ((method (lack.request:request-method request))
                      (path-info (lack.request:request-path-info request)))
                  (myway:dispatch mapper path-info :method method))
              (when foundp
                (funcall handler request))))
          (funcall app env)))))
