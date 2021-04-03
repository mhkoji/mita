(defpackage :mita.util.postgres
  (:use :cl)
  (:export :make-connector
           :connector
           :postgres
           :with-db
           :execute))
(in-package :mita.util.postgres)

(defstruct connector user host port)

(defun connector->spec (database connector)
  (list database
        (connector-user connector)
        "" ;; password
        (connector-host connector)
        :port (connector-port connector)
        :pooled-p t))

(defclass postgres ()
  ((conn
    :initarg :conn
    :reader postgres-conn)))

(defmacro with-db ((db db-name connector) &body body)
  `(postmodern:with-connection (connector->spec ,db-name ,connector)
     (postmodern:with-transaction (nil :serializable)
       (let ((,db (make-instance 'postgres :conn postmodern:*database*)))
         ,@body))))

(defun execute (db query-string args)
  (let ((conn (postgres-conn db)))
    (cl-postgres:prepare-query conn "" query-string)
    (cl-postgres:exec-prepared conn "" args #'cl-postgres:list-row-reader)))
