(defpackage :mita.account
  (:use :cl)
  (:export :with-db
           :account-root
           :account-thumbnail-root
           :create-account
           :delete-account))
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

(defun drop-account-database (account-id connector)
  (let ((db-name (account-id->db-name account-id)))
    (postmodern:with-connection
        (mita.util.postgres::connector->spec "admin" connector)
      (postmodern:query
       (format nil "DROP DATABASE IF EXISTS ~A" db-name)))))

(defun account-root (base account-id)
  (concatenate 'string base "/" (account-id->db-name account-id) "/"))

(defun create-account (account-id
                       connector
                       postgres-dir
                       content-base
                       thumbnail-base)
  (create-account-database postgres-dir account-id connector)
  (ensure-directories-exist (account-root content-base account-id))
  (ensure-directories-exist (account-root thumbnail-base account-id)))


(defun delete-account (account-id
                       connector
                       content-base
                       thumbnail-base)
  (drop-account-database account-id connector)
  (dolist (p (list (account-root content-base account-id)
                   (account-root thumbnail-base account-id)))
    (cl-fad:delete-directory-and-files p :if-does-not-exist :ignore)))
