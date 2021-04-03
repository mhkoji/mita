(defpackage :mita.admin
  (:use :cl)
  (:export :with-admin-db
           :create-account
           :delete-account
           :list-accounts
           :init))
(in-package :mita.admin)

(defmacro with-admin-db ((db connector) &body body)
  `(mita.db.impl:with-db (,db "admin" ,connector)
     ,@body))

(defun create-account (username password
                       connector
                       postgres-dir
                       content-base
                       thumbnail-base)
  (let ((account
         (with-admin-db (db connector)
           (mita.admin.account:create-account db username password))))
    (let ((id-string (mita.id:to-string
                      (mita.admin.account:account-id account))))
      (mita.account:create-account id-string
                                   connector
                                   postgres-dir
                                   content-base
                                   thumbnail-base))
    account))

(defun delete-account (account-id connector content-base thumbnail-base)
  (let ((id-string (mita.id:to-string account-id)))
    (mita.account:delete-account id-string
                                 connector
                                 content-base
                                 thumbnail-base))
  (with-admin-db (db connector)
    (mita.admin.account:delete-account db account-id)))

(defun list-accounts (connector)
  (with-admin-db (db connector)
    (mita.admin.account:load-accounts db)))

;;;

(defun init (connector postgres-dir content-base thumbnail-base)
  (mita.db.impl:create-admin-database postgres-dir "admin" connector)
  (create-account "mita" "mita"
                  connector postgres-dir
                  content-base
                  thumbnail-base))
