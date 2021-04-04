(defpackage :mita.util.mysql
  (:use :cl)
  (:export :call-with-tx
           :call-with-connection
           :execute))
(in-package :mita.util.mysql)

(defun call-with-tx (conn fn)
  (cl-dbi:do-sql conn
    "SET SESSION TRANSACTION ISOLATION LEVEL SERIALIZABLE")
  (cl-dbi:do-sql conn "START TRANSACTION")
  (handler-bind ((error
                   (lambda (c)
                     (declare (ignore c))
                     #+sbcl
                     (sb-debug:print-backtrace)
                     (cl-dbi:do-sql conn "ROLLBACK"))))
    (prog1
        (funcall fn)
      (cl-dbi:do-sql conn "COMMIT"))))

(defun call-with-connection (fn db-name username host port)
  (let ((conn (cl-dbi:connect
               :mysql
               :database-name db-name
               :username username
               :host host
               :port port)))
    (unwind-protect
         (funcall fn conn)
      (cl-dbi:disconnect conn))))

(defun execute (conn query-string args)
  (let ((q (cl-dbi:prepare conn query-string)))
    (dbi:fetch-all (dbi:execute q args))))
