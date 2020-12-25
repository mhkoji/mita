(defpackage :mita.postgres
  (:use :cl)
  (:export :make-connector
           :with-db
           :with-admin-db
           :create-account-database
           :init))
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

(defun account-db-name (account)
  (let ((id-string (mita.id:to-string
                    (mita.account:account-id account))))
    (format nil "account_~A"
            (string-downcase
             (cl-ppcre:regex-replace-all "-" id-string "_")))))

(defmacro with-db ((db account connector) &body body)
  `(mita.postgres.db:with-db (,db (connector->spec (account-db-name ,account)
                                                   ,connector))
     ,@body))

(defmacro with-admin-db ((db connector) &body body)
  `(mita.postgres.db:with-db (,db (connector->spec "admin" ,connector))
     ,@body))

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


(defun create-account (postgres-dir connector username password)
  (let ((account
         (with-admin-db (db connector)
           (mita.account:create-account db username password))))
    (create-account-database postgres-dir account connector)
    account))

(defun init (postgres-dir connector drop-p)
  (with-admin-db (db connector)
    (declare (ignore db))
    (when drop-p
      (mapc (lambda (q)
              (postmodern:execute q))
            (list "DROP SCHEMA public CASCADE;"
                  "CREATE SCHEMA public;"
                  "GRANT ALL ON SCHEMA public TO postgres;"
                  "GRANT ALL ON SCHEMA public TO public;"))))
  (with-admin-db (db connector)
    (declare (ignore db))
    (postmodern:execute-file
     (merge-pathnames postgres-dir "./account-ddl.sql")))
  (create-account postgres-dir connector "mita" "mita"))
