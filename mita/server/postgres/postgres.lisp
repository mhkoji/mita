(defpackage :mita.postgres
  (:use :cl)
  (:export :account-db-name
           :make-connector
           :with-db
           :with-admin-db
           :create-admin-database
           :create-account-database))
(in-package :mita.postgres)

(setq *read-eval* nil)

(defgeneric account-db-name (account))

(defstruct connector user host port)

(defun connector->spec (database connector)
  (list database
        (connector-user connector)
        "" ;; password
        (connector-host connector)
        :port (connector-port connector)
        :pooled-p t))

(defmacro with-db ((db account connector) &body body)
  `(mita.postgres.db:with-db (,db (connector->spec
                                   (account-db-name ,account)
                                   ,connector))
     ,@body))

(defmacro with-admin-db ((db connector) &body body)
  `(mita.postgres.db:with-db (,db (connector->spec "admin" ,connector))
     ,@body))

(defun create-admin-database (postgres-dir connector)
  (with-admin-db (db connector)
    (declare (ignore db))
    (postmodern:execute-file
     (merge-pathnames postgres-dir "./admin-ddl.sql"))))

(defun create-account-database (postgres-dir account connector)
  (postmodern:with-connection (list "admin"
                                    (connector-user connector)
                                    ""
                                    (connector-host connector)
                                    :port (connector-port connector))
    (postmodern:query
     (format nil "CREATE DATABASE ~A" (account-db-name account))))
  (postmodern:with-connection (list (account-db-name account)
                                    (connector-user connector)
                                    ""
                                    (connector-host connector)
                                    :port (connector-port connector))
    (postmodern:execute-file
     (merge-pathnames postgres-dir "./mita-ddl.sql"))))
