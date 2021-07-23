(defpackage :mita.auth.admin.account.rdb
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
(in-package :mita.auth.admin.account.rdb)

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


(defgeneric account-insert (conn account))

(defgeneric account-select (conn username))

(defgeneric account-select-all (conn))

(defgeneric account-select-by-id (conn account-id))

(defgeneric account-delete (conn account-id))
