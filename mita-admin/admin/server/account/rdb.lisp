(defpackage :mita.admin.account.rdb
  (:use :cl)
  (:export :make-account
           :account
           :account-id
           :account-username
           :account-hashed-password
           :account-select
           :account-select-all
           :account-select-by-id
           :account-insert
           :account-delete)
  (:import-from :alexandria
                :when-let))
(in-package :mita.admin.account.rdb)

;; Use defclass instead of defstruct so that the hashed password of an object is not printed accidentally.
(defclass account ()
  ((id
    :initarg :id
    :reader account-id)
   (username
    :initarg :username
    :reader account-username)
   (hashed-password
    :initarg :hashed-password
    :reader account-hashed-password)))

(defun make-account (&key id username hashed-password)
  (make-instance 'account
                 :id id
                 :username username
                 :hashed-password hashed-password))


(defgeneric account-insert (db account))

(defgeneric account-select (db username))

(defgeneric account-select-all (db))

(defgeneric account-select-by-id (db account-id))

(defgeneric account-delete (db account-id))
