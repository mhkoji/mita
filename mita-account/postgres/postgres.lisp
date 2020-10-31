(defpackage :mita.account.postgres
  (:use :cl)
  (:export :create-tables))
(in-package :mita.account.postgres)

(defun create-tables ()
  (postmodern:execute-file
   (asdf:system-relative-pathname
    (asdf:find-system :mita-account)
    "./postgres/ddl.sql")))
