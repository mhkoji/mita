(defpackage :mita.account.db
  (:use :cl)
  (:export :make-account
           :account
           :account-id
           :account-username
           :account-hashed-password
           :account-select
           :account-select-by-id
           :account-insert
           :make-hashed-password
           :hash-password
           :hashed-password-string
           :hashed-password-matches-p))
(in-package :mita.account.db)

(defstruct hashed-password string)

(defun hash-password (raw)
  (make-hashed-password
   :string
   (cl-bcrypt:encode
    (cl-bcrypt:make-password raw))))

(defun hashed-password-matches-p (hashed password)
  (cl-bcrypt:password= password (hashed-password-string hashed)))


;;;;;;;;;;;;;;;;;;;;;;

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
