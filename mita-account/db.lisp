(defpackage :mita.account.db
  (:use :cl)
  (:export :make-account
           :account-id
           :account-name
           :account-hashed-password
           :account-select
           :account-insert
           :hash-password
           :hashed-password-matches-p)
  (:import-from :mita.db.postgres
                :postgres))
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
  (make-instance 'account
                 :id id
                 :username username
                 :hashed-password hashed-password))

(defgeneric account-insert (db account))

(defgeneric account-select (db username))


;;;; postgres

(defmethod account-insert ((db postgres) (account account))
  (mita.db.postgres::insert-into
   db "accounts" '("account_id" "username" "password_hashed")
   (list (list (mita.id:to-string (account-id account))
               (account-username account)
               (hashed-password-string
                (account-hashed-password account))))))

(defmethod account-select ((db postgres) (username string))
  (labels ((parse-account (row)
             (make-account
              :id (mita.id:parse (first row))
              :username (second row)
              :hashed-password (make-hashed-password
                                :string (third row)))))
    (mita.db.postgres::single
     #'parse-account
     (mita.db.postgres::select-from
      db "account_id, username, password_hashed" "accounts"
      `(:where (:= "username" (:p ,username)))))))
