(defpackage :mita.db.impl
  (:use :cl))
(in-package :mita.db.impl)

(defun reexport (syms)
  (dolist (sym syms)
    (import sym)
    (export (intern (string sym)))))

#+nil
(progn
  (defun make-locator ()
    (mita.db.rdb.vendor.postgres:make-locator
     :user "postgres"
     :host "localhost"
     :port 5432))

  (defun make-db (db-name locator)
    (make-instance 'mita.db.rdb.vendor.postgres:postgres
                   :db-name db-name
                   :locator locator))

  (reexport '(make-locator
              make-db
              mita.db.rdb.vendor.postgres:create-admin-database
              mita.db.rdb.vendor.postgres:create-database
              mita.db.rdb.vendor.postgres:drop-database)))

(progn
  (defun make-locator ()
    (mita.db.rdb.vendor.mysql:make-locator
     :user "root"
     :host "127.0.0.1"
     :port 3306))

  (defun make-db (db-name locator)
    (make-instance 'mita.db.rdb.vendor.mysql:mysql
                   :db-name db-name
                   :locator locator))


  (reexport '(make-locator
              make-db
              mita.db.rdb.vendor.mysql:create-admin-database
              mita.db.rdb.vendor.mysql:create-database
              mita.db.rdb.vendor.mysql:drop-database)))

#+nil
(progn
  (defun make-locator ()
    "/data/content/")

  (defun make-db (db-name locator)
    (make-instance 'mita.db.file:file-db
                   :db-name db-name
                   :base-dir locator))

  (reexport '(make-locator
              make-db
              mita.db.file:create-admin-database
              mita.db.file:create-database
              mita.db.file:drop-database)))
