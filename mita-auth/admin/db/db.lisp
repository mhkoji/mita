(defpackage :mita.auth.admin.db
  (:use :cl)
  (:export :db-manager-db-dir
           :mysql-manager
           :postgres-manager
           :get-locator
           :get-db
           :create-admin-database
           :create-database
           :drop-database))
(in-package :mita.auth.admin.db)

(defclass db-manager ()
  ((db-dir
    :initarg :db-dir
    :reader db-manager-db-dir)))

(defgeneric get-locator (db-manager))

(defgeneric get-db (db-manager db-name))

(defgeneric create-admin-database (db-manager db-name))

(defgeneric create-database (db-maanger db-name))

(defgeneric drop-database (db-manager db-name))

(defclass mysql-manager (db-manager)
  ((locator
    :initarg :locator
    :initform (mita.db.vendor.mysql:make-locator
               :user "root"
               :host "127.0.0.1"
               :port 3306)
    :reader get-locator)))

(defclass postgres-manager (db-manager)
  ((locator
    :initarg :locator
    :initform (mita.db.vendor.postgres:make-locator
               :user "postgres"
               :host "localhost"
               :port 5432)
    :reader get-locator)))
