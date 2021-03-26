(defpackage :mita.postgres
  (:use :cl)
  (:export :make-connector
           :connector
           :with-db
           :with-admin-db
           :create-admin-database
           :create-account-database))
(in-package :mita.postgres)

(setq *read-eval* nil)

(defstruct connector user host port)

(defun connector->spec (database connector)
  (list database
        (connector-user connector)
        "" ;; password
        (connector-host connector)
        :port (connector-port connector)
        :pooled-p t))

(defun account-id->db-name (id-string)
  (format nil "account_~A"
          (string-downcase (cl-ppcre:regex-replace-all "-" id-string "_"))))

(defmacro with-db ((db account-id connector) &body body)
  `(mita.postgres.db:with-db (,db (connector->spec
                                   (account-id->db-name ,account-id)
                                   ,connector))
     ,@body))

;;;

(defmacro with-admin-db ((db connector) &body body)
  `(mita.postgres.db:with-db (,db (connector->spec "admin" ,connector))
     ,@body))

(defun create-admin-database (postgres-dir connector)
  (with-admin-db (db connector)
    (declare (ignore db))
    (postmodern:execute-file
     (merge-pathnames postgres-dir "./admin-ddl.sql"))))

(defun create-account-database (postgres-dir account-id connector)
  (let ((db-name (account-id->db-name account-id)))
    (postmodern:with-connection (list "admin"
                                      (connector-user connector)
                                      ""
                                      (connector-host connector)
                                      :port (connector-port connector))
      (postmodern:query
       (format nil "CREATE DATABASE ~A" db-name)))
    (postmodern:with-connection (list db-name
                                      (connector-user connector)
                                      ""
                                      (connector-host connector)
                                      :port (connector-port connector))
      (postmodern:execute-file
       (merge-pathnames postgres-dir "./mita-ddl.sql")))))
