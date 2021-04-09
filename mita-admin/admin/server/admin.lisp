(defpackage :mita.admin
  (:use :cl)
  (:export :make-admin-rdb
           :create-account
           :delete-account
           :list-accounts
           :init))
(in-package :mita.admin)

(defun make-admin-rdb (locator)
  (mita.rdb.impl:make-rdb "admin" locator))

(defun create-account (username password
                       locator
                       postgres-dir
                       content-base
                       thumbnail-base)
  (let ((account
         (mita.rdb:with-connection (conn (make-admin-rdb locator))
           (mita.rdb:with-tx (conn)
             (mita.admin.account:create-account conn username password)))))
    (let ((id-string (mita.id:to-string
                      (mita.admin.account:account-id account))))
      (mita.account:create-account id-string
                                   locator
                                   postgres-dir
                                   content-base
                                   thumbnail-base))
    account))

(defun delete-account (account-id locator content-base thumbnail-base)
  (let ((id-string (mita.id:to-string account-id)))
    (mita.account:delete-account id-string
                                 locator
                                 content-base
                                 thumbnail-base))
  (mita.rdb:with-connection (conn (make-admin-rdb locator))
    (mita.admin.account:delete-account conn account-id)))

(defun list-accounts (locator)
  (mita.rdb:with-connection (conn (make-admin-rdb locator))
    (mita.admin.account:load-accounts conn)))

;;;

(defun init (locator postgres-dir content-base thumbnail-base)
  (mita.rdb.impl:create-admin-database postgres-dir "admin" locator)
  (create-account "mita" "mita"
                  locator postgres-dir
                  content-base
                  thumbnail-base))
