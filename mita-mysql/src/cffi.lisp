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
  (:unix "libmysqlclient.so"))

(cffi:use-foreign-library libmysqlclient)

(cffi:defcfun ("mysql_init" mysql-init) :pointer
  (mysql :pointer))

(cffi:defcfun ("mysql_close" mysql-close) :pointer
  (mysql :pointer))

(cffi:defcfun ("mysql_error" mysql-error) :string
  (mysql :pointer))

(cffi:defcfun ("mysql_errno" mysql-errno) :unsigned-int
  (mysql :pointer))

(cffi:defcfun ("mysql_query" mysql-query) :int
  (mysql :pointer)
  (statement :string))

(cffi:defcfun ("mysql_real_connect" mysql-real-connect) :pointer
  (mysql :pointer)
  (host :string)
  (user :string)
  (password :string)
  (database :string)
  (port :int)
  (unix-socket :string)
  (client-flag :unsigned-long))

(cffi:defcfun ("mysql_free_result" mysql-free-result) :void
  (mysql-res :pointer))


(cffi:defcfun ("mysql_num_fields" mysql-num-fields) :unsigned-int
  (mysql-res :pointer))

(cffi:defcfun ("mysql_fetch_fields" mysql-fetch-fields) :pointer
  (mysql-res :pointer))

(cffi:defcfun ("mysql_stmt_result_metadata" mysql-stmt-result-metadata)
    :pointer
  (mysql-stmt :pointer))

(cffi:defcfun ("mysql_stmt_init" mysql-stmt-init) :pointer
  (mysql :pointer))

(cffi:defcfun ("mysql_stmt_prepare" mysql-stmt-prepare) :int
  (stmt :pointer))

(cffi:defcfun ("mysql_stmt_prepare" mysql-stmt-prepare) :int
  (stmt :pointer)
  (stmt-str :string)
  (length :unsigned-long))

(cffi:defcfun ("mysql_stmt_bind_param" mysql-stmt-bind-param) :boolean
  (stmt :pointer)
  (bind :pointer))

(cffi:defcfun ("mysql_stmt_bind_result" mysql-stmt-bind-result) :boolean
  (stmt :pointer)
  (bind :pointer))

(cffi:defcfun ("mysql_stmt_next_result" mysql-stmt-next-result) :int
  (stmt :pointer))

(cffi:defcfun ("mysql_stmt_execute" mysql-stmt-execute) :int
  (stmt :pointer))

(cffi:defcfun ("mysql_stmt_fetch" mysql-stmt-fetch) :int
  (stmt :pointer))

(cffi:defcfun ("mysql_stmt_close" mysql-stmt-close) :boolean
  (stmt :pointer))

(cffi:defcstruct mysql-field
  (name :string)
  (org-name :string)
  (table :string)
  (org-table :string)
  (db :string)
  (catalog :string)
  (def :string)
  (length :unsigned-long)
  (max-length :unsigned-long)
  (name-length :unsigned-int)
  (org-name-length :unsigned-int)
  (table-length :unsigned-int)
  (org-table-length :unsigned-int)
  (db-length :unsigned-int)
  (catalog-length :unsigned-int)
  (def-length :unsigned-int)
  (flags :unsigned-int)
  (decimals :unsigned-int)
  (charsetnr :unsigned-int)
  (type :int)
  (extension :pointer))

(cffi:defcstruct mysql-bind
  (length :pointer)
  (is-null :pointer)
  (buffer :pointer)
  (error :pointer)
  (row-ptr :pointer)
  (store-param-func :pointer)
  (fetch-result-func :pointer)
  (skip-result-func :pointer)
  (buffer-length :unsigned-long)
  (offset :unsigned-long)
  (length-value :unsigned-long)
  (param-number :unsigned-int)
  (pack-length :unsigned-int)
  (buffer-type :int)
  (error-value :char)
  (is-unsigned :char)
  (long-data-used :char)
  (is-null-value :char)
  (extension :pointer))

(cffi:defcenum enum-field-types
  :decimal
  :tiny
  :short
  :long
  :float
  :double
  :null
  :timestamp
  :longlong
  :int24
  :date
  :time
  :datetime
  :year
  :newdate
  :varchar
  :bit
  (:var-string 253)
  (:string 254))
