(defpackage :mita.account.db
  (:use :cl)
  (:export :make-account
           :account-id
           :account-name
           :account-select
           :account-insert
           :hash-password
           :password-hashed-string)
  (:import-from :mita.db.postgres
                :postgres))
(in-package :mita.account.db)

(defstruct password-hashed string)

(defun hash-password (raw)
  (make-password-hashed
   :string
   ;; Should be replaced with another like bcrypt
   (ironclad:byte-array-to-hex-string
    (ironclad:digest-sequence
     :sha256
     (ironclad:ascii-string-to-byte-array raw)))))


;;;;;;;;;;;;;;;;;;;;;;

(defstruct account id username)

(defgeneric account-insert (db account password-hashed))

(defgeneric account-select (db username password-hashed))


;;;; postgres

(defmethod account-insert ((db postgres)
                           (account account)
                           (password password-hashed))
  (mita.db.postgres::insert-into
   db "accounts" '("account_id" "username" "password_hashed")
   (list (list (mita.id:to-string (account-id account))
               (account-username account)
               (password-hashed-string password)))))

(defmethod account-select ((db postgres)
                           (username string)
                           (password password-hashed))
  (labels ((parse-account (row)
             (make-account :id (mita.id:parse (first row))
                           :username (second row))))
    (mita.db.postgres::single
     #'parse-account
     (mita.db.postgres::select-from
      db "account_id, username" "accounts"
      `(:where (:and (:= "username"
                         (:p ,username))
                     (:= "password_hashed"
                         (:p ,(password-hashed-string password)))))))))
