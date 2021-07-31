(defpackage :mita.db
  (:use :cl)
  (:export :db
           :connection
           :call-with-connection
           :call-with-tx
           :with-connection
           :with-tx))
(in-package :mita.db)

(defclass db () ())

(defclass connection () ())

(defgeneric call-with-connection (db fn))

(defgeneric call-with-tx (conn fn))

(defmacro with-connection ((conn db) &body body)
  `(call-with-connection ,db (lambda (,conn) ,@body)))

(defmacro with-tx ((conn) &body body)
  `(call-with-tx ,conn (lambda () ,@body)))
