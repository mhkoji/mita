(defpackage :mita.admin.account.db
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
           :account-delete))
(in-package :mita.admin.account.db)

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
  (make-instance 'mita.admin.account.db:account
                 :id id
                 :username username
                 :hashed-password hashed-password))


(defgeneric account-insert (db account))

(defgeneric account-select (db username))

(defgeneric account-select-all (db))

(defgeneric account-delete (db account-id))
