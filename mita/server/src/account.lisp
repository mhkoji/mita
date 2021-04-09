(defpackage :mita.account
  (:use :cl)
  (:export :make-rdb
           :account-root
           :account-thumbnail-root
           :create-account
           :delete-account))
(in-package :mita.account)

(defun account-id->rdb-name (id-string)
  (format nil "account_~A"
          (string-downcase
           (cl-ppcre:regex-replace-all "-" id-string "_"))))

(defun make-rdb (account-id locator)
  (mita.rdb.impl:make-rdb (account-id->rdb-name account-id)
                          locator))

(defun create-account-database (rdb-dir account-id locator)
  (let ((rdb-name (account-id->rdb-name account-id)))
    (mita.rdb.impl:create-database rdb-dir "admin" rdb-name locator)))

(defun drop-account-database (account-id locator)
  (let ((rdb-name (account-id->rdb-name account-id)))
    (mita.rdb.impl:drop-database "admin" rdb-name locator)))


(defun account-root (base account-id)
  (concatenate 'string base "/" (account-id->rdb-name account-id) "/"))

(defun create-account (account-id
                       locator
                       postgres-dir
                       content-base
                       thumbnail-base)
  (create-account-database postgres-dir account-id locator)
  (ensure-directories-exist (account-root content-base account-id))
  (ensure-directories-exist (account-root thumbnail-base account-id)))


(defun delete-account (account-id
                       locator
                       content-base
                       thumbnail-base)
  (drop-account-database account-id locator)
  (dolist (p (list (account-root content-base account-id)
                   (account-root thumbnail-base account-id)))
    (cl-fad:delete-directory-and-files p :if-does-not-exist :ignore)))
