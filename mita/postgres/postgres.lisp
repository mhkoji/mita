(defpackage :mita.postgres
  (:use :cl)
  (:export :make-connector
           :with-gateway
           :with-admin-gateway
           :create-account-database
           :create-account-tables))
(in-package :mita.postgres)

(defstruct connector user host port)

(defun account-db-name (account)
  (let ((id-string (mita.id:to-string
                    (mita.account:account-id account))))
    (format nil "account_~A"
            (string-downcase
             (cl-ppcre:regex-replace-all "-" id-string "_")))))

(defmacro with-gateway ((gw account connector) &body body)
  (let ((g (gensym)))
    `(mita.postgres.db:with-transaction
         (,g :database (account-db-name ,account)
             :user     (connector-user ,connector)
             :host     (connector-host ,connector)
             :port     (connector-port ,connector))
       (let ((,gw (make-instance 'mita:gateway :db ,g)))
         ,@body))))


(defmacro with-admin-gateway ((gw connector) &body body)
  (let ((g (gensym)))
    `(mita.postgres.db:with-transaction
         (,g :database "admin"
             :user     (connector-user ,connector)
             :host     (connector-host ,connector)
             :port     (connector-port ,connector))
       (let ((,gw (make-instance 'mita:gateway :db ,g)))
         ,@body))))

(defun create-account-tables (postgres-dir)
  (postmodern:execute-file
   (merge-pathnames postgres-dir "./account-ddl.sql")))

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
     (merge-pathnames postgres-dir "./postgres/mita-ddl.sql"))))
