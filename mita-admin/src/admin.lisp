(defpackage :mita.admin
  (:use :cl)
  (:export :init
           :with-admin-db))
(in-package :mita.admin)

(defmacro with-admin-db ((db connector) &body body)
  (let ((g (gensym)))
    `(mita.util.postgres:with-db (,g "admin" ,connector)
       (let ((,db (change-class ,g 'mita.db.postgres:postgres)))
         ,@body))))

(defun create-admin-database (postgres-dir connector)
  (with-admin-db (db connector)
    (declare (ignore db))
    (postmodern:execute-file
     (merge-pathnames postgres-dir "./admin-ddl.sql"))))

(defun create-account (postgres-dir connector username password)
  (let ((account
         (with-admin-db (db connector)
           (mita.account:create-account db username password))))
    (mita.db.postgres:create-account-database
     postgres-dir
     (mita.id:to-string (mita.account:account-id account))
     connector)
    account))

(defun init (postgres-dir connector drop-p)
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
  (create-account postgres-dir connector "mita" "mita"))
