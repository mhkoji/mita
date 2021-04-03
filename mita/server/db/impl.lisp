(defpackage :mita.db.impl
  (:use :cl))
(in-package :mita.db.impl)

(defun reexport (syms)
  (dolist (sym syms)
    (import sym)
    (export (intern (string sym)))))

#+nil
(progn
  (defun make-connector ()
    (mita.util.postgres:make-connector
     :user "postgres"
     :host "localhost"
     :port 5432))

  (reexport '(make-connector
              mita.db.postgres:with-db
              mita.db.postgres:create-admin-database
              mita.db.postgres:create-database
              mita.db.postgres:drop-database)))

(progn
  (defun make-connector ()
    (mita.util.mysql:make-connector
     :user "root"
     :host "127.0.0.1"
     :port 3306))

  (reexport '(make-connector
              mita.db.mysql::with-db
              mita.db.mysql:create-admin-database
              mita.db.mysql:create-database
              mita.db.mysql:drop-database)))

#+nil
(progn
  (defun make-connector ()
    (mita.db.file:make-connector :dir "./db-file/"))

  (reexport '(make-connector
              mita.db.file:with-db)))
