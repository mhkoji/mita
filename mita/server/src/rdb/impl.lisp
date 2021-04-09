(defpackage :mita.rdb.impl
  (:use :cl))
(in-package :mita.rdb.impl)

(defun reexport (syms)
  (dolist (sym syms)
    (import sym)
    (export (intern (string sym)))))

#+nil
(progn
  (defun make-locator ()
    (mita.rdb.postgres:make-locator
     :user "postgres"
     :host "localhost"
     :port 5432))

  (defun make-rdb (rdb-name locator)
    (make-instance 'mita.rdb.postgres:postgres
                   :rdb-name rdb-name
                   :locator locator))

  (reexport '(make-locator
              make-rdb
              mita.rdb.postgres:create-admin-database
              mita.rdb.postgres:create-database
              mita.rdb.postgres:drop-database)))

(progn
  (defun make-locator ()
    (mita.rdb.mysql:make-locator
     :user "root"
     :host "127.0.0.1"
     :port 3306))

  (defun make-rdb (rdb-name locator)
    (make-instance 'mita.rdb.mysql:mysql
                   :rdb-name rdb-name
                   :locator locator))


  (reexport '(make-locator
              make-rdb
              mita.rdb.mysql:create-admin-database
              mita.rdb.mysql:create-database
              mita.rdb.mysql:drop-database)))
