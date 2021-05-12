(defpackage :mita.admin
  (:use :cl)
  (:export :make-admin-db
           :create-account
           :delete-account
           :list-accounts
           :init))
(in-package :mita.admin)

(defun make-admin-db (locator)
  (mita.db.impl:make-db "admin" locator))

(defun create-account (username password
                       locator
                       postgres-dir
                       content-base
                       thumbnail-base
                       &key (id (mita.id:gen)))
  (let ((account
         (mita.db:with-connection (conn (make-admin-db locator))
           (mita.db:with-tx (conn)
             (mita.admin.account:create-account
              conn id username password)))))
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
  (mita.db:with-connection (conn (make-admin-db locator))
    (mita.admin.account:delete-account conn account-id)))

(defun list-accounts (locator)
  (mita.db:with-connection (conn (make-admin-db locator))
    (mita.admin.account:load-accounts conn)))

;;;

(defun init (locator postgres-dir content-base thumbnail-base)
  (mita.db.impl:create-admin-database postgres-dir "admin" locator)
  (let ((id (mita.id:parse "7128DA4E-2B13-45CC-A0BF-78AEC1668E2C")))
    (create-account "mita" "mita"
                    locator postgres-dir
                    content-base
                    thumbnail-base
                    :id id)))
