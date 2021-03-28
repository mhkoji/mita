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

(defun find-account-with-password-checked (db username password)
  (when-let ((row (mita.admin.account.db:account-select db username)))
    (when (mita.util.password:hashed-password-matches-p
           (mita.admin.account.db:account-hashed-password row)
           password)
      (make-instance 'account :row row))))

(defun find-account (db username)
  (when-let ((row (mita.admin.account.db:account-select db username)))
    (make-instance 'account :row row)))

(defun find-account-by-id (db account-id)
  (when-let ((row (mita.admin.account.db:account-select-by-id
                   db account-id)))
    (make-instance 'account :row row)))

(defun create-account (db username password)
  (let ((account (mita.admin.account.db:make-account
                  :id (mita.id:gen)
                  :username username
                  :hashed-password
                  (mita.util.password:hash password))))
    (mita.admin.account.db:account-insert db account))
  (find-account db username))

(defun delete-account (db account-id)
  (mita.admin.account.db:account-delete db account-id))

(defun load-accounts (db)
  (mapcar (lambda (row)
            (make-instance 'account :row row))
          (mita.admin.account.db:account-select-all db)))
