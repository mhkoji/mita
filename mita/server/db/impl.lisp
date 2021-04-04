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
    (mita.db.postgres:make-locator
     :user "postgres"
     :host "localhost"
     :port 5432))

  (defun make-db (db-name locator)
    (make-instance 'mita.db.postgres:postgres
                   :db-name db-name
                   :locator locator))

  (reexport '(make-locator
              make-db
              mita.db.postgres:create-admin-database
              mita.db.postgres:create-database
              mita.db.postgres:drop-database)))

(progn
  (defun make-locator ()
    (mita.db.mysql:make-locator
     :user "root"
     :host "127.0.0.1"
     :port 3306))

  (defun make-db (db-name locator)
    (make-instance 'mita.db.mysql:mysql
                   :db-name db-name
                   :locator locator))


  (reexport '(make-locator
              make-db
              mita.db.mysql:create-admin-database
              mita.db.mysql:create-database
              mita.db.mysql:drop-database)))

#+nil
(progn
  (defun make-locator ()
    (mita.db.file:make-locator :dir "./db-file/"))

  (reexport '(make-locator
              mita.db.file:with-db)))
