(defpackage :mita.account
  (:use :cl)
  (:export :with-db
           :account-content-root
           :create-account))
(in-package :mita.account)

(defun account-id->db-name (id-string)
  (format nil "account_~A"
          (string-downcase (cl-ppcre:regex-replace-all "-" id-string "_"))))

(defmacro with-db ((db account-id connector) &body body)
  `(mita.db.impl:with-db (,db (account-id->db-name ,account-id) ,connector)
     ,@body))

(defun create-account-database (postgres-dir account-id connector)
  (let ((db-name (account-id->db-name account-id)))
    (postmodern:with-connection
        (mita.util.postgres::connector->spec "admin" connector)
      (postmodern:query
       (format nil "CREATE DATABASE ~A" db-name)))
    (postmodern:with-connection
        (mita.util.postgres::connector->spec db-name connector)
      (postmodern:execute-file
       (merge-pathnames postgres-dir "./mita-ddl.sql")))))

(defun account-content-root (account-id account-content-base-dir)
  (concatenate 'string
               account-content-base-dir
               (account-id->db-name account-id)
               "/"))

(defun create-account (account-id
                       connector
                       postgres-dir
                       account-content-base-dir)
  (create-account-database postgres-dir account-id connector)
  (ensure-directories-exist (account-content-root
                             account-id
                             account-content-base-dir)))
