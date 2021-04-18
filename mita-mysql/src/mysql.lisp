(defpackage :mita-mysql
  (:use :cl))
(in-package :mita-mysql)

(defclass connection ()
  ((mysql
    :initarg :mysql
    :reader connection-mysql)))

(defun connect (hostname username password database port flags)
  (let ((mysql (mita-mysql.cffi:mysql-real-connect
                (mita-mysql.cffi:mysql-init (cffi:null-pointer))
                (or hostname "127.0.0.1")
                (or username (cffi:null-pointer))
                (or password (cffi:null-pointer))
                (or database (cffi:null-pointer))
                (or port 0)
                (cffi:null-pointer)
                (or flags 196640))))
    (make-instance 'connection :mysql mysql)))

(defun disconnect (conn)
  (mita-mysql.cffi:mysql-close (connection-mysql conn)))
