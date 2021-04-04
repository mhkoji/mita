(defpackage :mita.admin.account
  (:use :cl)
  (:export :account
           :account-id
           :account-username
           :find-account-with-password-checked
           :find-account
           :find-account-by-id
           :create-account
           :delete-account
           :load-accounts)
  (:import-from :alexandria
                :when-let))
(in-package :mita.admin.account)

(defclass account ()
  ((row :initarg :row)))

(defun account-id (account)
  (mita.admin.account.db:account-id (slot-value account 'row)))

(defun account-username (account)
  (mita.admin.account.db:account-username (slot-value account 'row)))

(defun find-account-with-password-checked (conn username password)
  (when-let ((row (mita.admin.account.db:account-select conn username)))
    (when (mita.util.password:hashed-password-matches-p
           (mita.admin.account.db:account-hashed-password row)
           password)
      (make-instance 'account :row row))))

(defun find-account (conn username)
  (when-let ((row (mita.admin.account.db:account-select conn username)))
    (make-instance 'account :row row)))

(defun find-account-by-id (conn account-id)
  (when-let ((row (mita.admin.account.db:account-select-by-id
                   conn account-id)))
    (make-instance 'account :row row)))

(defun create-account (conn username password)
  (let ((account (mita.admin.account.db:make-account
                  :id (mita.id:gen)
                  :username username
                  :hashed-password
                  (mita.util.password:hash password))))
    (mita.admin.account.db:account-insert conn account))
  (find-account conn username))

(defun delete-account (conn account-id)
  (mita.admin.account.db:account-delete conn account-id))

(defun load-accounts (conn)
  (mapcar (lambda (row)
            (make-instance 'account :row row))
          (mita.admin.account.db:account-select-all conn)))
