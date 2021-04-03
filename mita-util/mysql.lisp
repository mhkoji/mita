(defpackage :mita.util.mysql
  (:use :cl)
  (:export :make-connector
           :connector
           :mysql
           :with-db
           :execute))
(in-package :mita.util.mysql)

(defstruct connector user host port)

(defclass mysql ()
  ((conn
    :initarg :conn
    :reader mysql-conn)))

(defmacro with-db ((db db-name connector) &body body)
  (let ((conn (gensym)))
    `(let ((,conn (cl-dbi:connect
                   :mysql
                   :database-name ,db-name
                   :username (connector-user ,connector)
                   :host (connector-host ,connector)
                   :port (connector-port ,connector))))
       (unwind-protect
            (progn
              (cl-dbi:do-sql ,conn
                "SET SESSION TRANSACTION ISOLATION LEVEL SERIALIZABLE")
              (cl-dbi:do-sql ,conn "START TRANSACTION")
              (handler-case
                  (prog1
                    (let ((,db (make-instance 'mysql :conn ,conn)))
                      ,@body)
                    (cl-dbi:do-sql ,conn "COMMIT"))
                (error ()
                  (cl-dbi:do-sql ,conn "ROLLBACK"))))
         (cl-dbi:disconnect ,conn)))))

(defun execute (db query-string args)
  (let ((q (cl-dbi:prepare (mysql-conn db) query-string)))
    (dbi:fetch-all (dbi:execute q args))))
