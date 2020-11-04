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

(defun find-account-with-password-checked (gw username password)
  (when-let ((row (mita.account.db:account-select
                   (mita:gateway-db gw)
                   username)))
    (when (mita.account.db:hashed-password-matches-p
           (mita.account.db:account-hashed-password row)
           password)
      (make-instance 'account :row row))))

(defun find-account (gw username)
  (when-let ((row (mita.account.db:account-select
                   (mita:gateway-db gw)
                   username)))
    (make-instance 'account :row row)))

(defun find-account-by-id (gw account-id)
  (when-let ((row (mita.account.db:account-select-by-id
                   (mita:gateway-db gw)
                   account-id)))
    (make-instance 'account :row row)))

(defun create-account (gw username password)
  (let ((db (mita:gateway-db gw))
        (account (mita.account.db:make-account
                  :id (mita.id:gen)
                  :username username
                  :hashed-password
                  (mita.account.db:hash-password password))))
    (mita.account.db:account-insert db account))
  (find-account gw username))
