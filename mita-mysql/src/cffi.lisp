(defpackage :mita-mysql.cffi
  (:use :cl)
  (:export :mysql-init
           :mysql-close
           :mysql-error
           :mysql-errno
           :mysql-query
           :mysql-real-connect))
(in-package :mita-mysql.cffi)

(cffi:define-foreign-library libmysqlclient
  (:unix "libmysqlclient"))

(cffi:defcfun ("mysql_init" mysql-init) :pointer
  (mysql :pointer))

(cffi:defcfun ("mysql_close" mysql-close) :pointer
  (mysql :pointer))

(cffi:defcfun ("mysql_error" mysql-error) :string
  (mysql :pointer))

(cffi:defcfun ("mysql_errno" mysql-errno) :unsigned-int
  (mysql :pointer))

(cffi:defcfun ("mysql_real_connect" mysql-real-connect) :pointer
  (mysql :pointer)
  (host :string)
  (user :string)
  (password :string)
  (database :string)
  (port :int)
  (unix-socket :string)
  (client-flag :unsigned-long))
