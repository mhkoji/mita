(defpackage :mita.account.db
  (:use :cl)
  (:export :make-account
           :account
           :account-id
           :account-username
           :account-hashed-password
           :account-select
           :account-select-by-id
           :account-insert))
(in-package :mita.account.db)

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
  (make-instance 'mita.account.db:account
                 :id id
                 :username username
                 :hashed-password hashed-password))


(defgeneric account-insert (db account))

(defgeneric account-select (db username))
