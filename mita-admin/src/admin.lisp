(defpackage :mita.admin
  (:use :cl)
  (:export :with-admin-db
           :create-account
           :list-accounts
           :init))
(in-package :mita.admin)

(defmacro with-admin-db ((db connector) &body body)
  `(mita.db.impl:with-db (,db "admin" ,connector)
     ,@body))

(defun create-admin-database (postgres-dir connector)
  (with-admin-db (db connector)
    (declare (ignore db))
    (postmodern:execute-file
     (merge-pathnames postgres-dir "./admin-ddl.sql"))))

(defun create-account (username password
                       connector
                       postgres-dir
                       account-content-base-dir)
  (let ((account
         (with-admin-db (db connector)
           (mita.admin.account:create-account db username password))))
    (let ((id-string (mita.id:to-string
                      (mita.admin.account:account-id account))))
      (mita.account:create-account id-string
                                   connector
                                   postgres-dir
                                   account-content-base-dir))
    account))

(defun list-accounts (connector)
  (with-admin-db (db connector)
    (mita.admin.account:load-accounts db)))

;;;

(defun init (connector postgres-dir account-content-base-dir drop-p)
  (when drop-p
    (with-admin-db (db connector)
      (declare (ignore db))
      (mapc (lambda (q)
              (postmodern:execute q))
            (list "DROP SCHEMA public CASCADE;"
                  "CREATE SCHEMA public;"
                  "GRANT ALL ON SCHEMA public TO postgres;"
                  "GRANT ALL ON SCHEMA public TO public;"))))
  (create-admin-database postgres-dir connector)
  (create-account "mita" "mita"
                  connector postgres-dir
                  account-content-base-dir))
