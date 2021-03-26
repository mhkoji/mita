(defpackage :mita.admin
  (:use :cl)
  (:export :init))
(in-package :mita.admin)

(defun create-account (postgres-dir connector username password)
  (let ((account
         (mita.postgres:with-admin-db (db connector)
           (mita.account:create-account db username password))))
    (mita.postgres:create-account-database
     postgres-dir
     (mita.id:to-string (mita.account:account-id account))
     connector)
    account))

(defun init (postgres-dir connector drop-p)
  (when drop-p
    (mita.postgres:with-admin-db (db connector)
      (declare (ignore db))
      (mapc (lambda (q)
              (postmodern:execute q))
            (list "DROP SCHEMA public CASCADE;"
                  "CREATE SCHEMA public;"
                  "GRANT ALL ON SCHEMA public TO postgres;"
                  "GRANT ALL ON SCHEMA public TO public;"))))
  (mita.postgres:create-admin-database postgres-dir connector)
  (create-account postgres-dir connector "mita" "mita"))
