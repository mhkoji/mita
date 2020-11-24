(defpackage :mita.account
  (:use :cl)
  (:export :account
           :account-id
           :find-account-with-password-checked
           :find-account
           :find-account-by-id
           :create-account)
  (:import-from :alexandria
                :when-let))
(in-package :mita.account)

(defclass account ()
  ((row :initarg :row)))

(defmethod account-id ((a account))
  (mita.account.db:account-id (slot-value a 'row)))

(defun find-account-with-password-checked (db username password)
  (when-let ((row (mita.account.db:account-select db username)))
    (when (mita.account.db:hashed-password-matches-p
           (mita.account.db:account-hashed-password row)
           password)
      (make-instance 'account :row row))))

(defun find-account (db username)
  (when-let ((row (mita.account.db:account-select db username)))
    (make-instance 'account :row row)))

(defun find-account-by-id (db account-id)
  (when-let ((row (mita.account.db:account-select-by-id
                   db account-id)))
    (make-instance 'account :row row)))

(defun create-account (db username password)
  (let ((account (mita.account.db:make-account
                  :id (mita.id:gen)
                  :username username
                  :hashed-password
                  (mita.account.db:hash-password password))))
    (mita.account.db:account-insert db account))
  (find-account db username))
