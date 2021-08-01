(defpackage :mita.auth.admin.account
  (:use :cl)
  (:export :account
           :account-id
           :account-username
           :construct-account
           :find-account-with-password-checked
           :find-account
           :find-account-by-id
           :create-account
           :delete-account
           :load-accounts)
  (:import-from :alexandria
                :when-let))
(in-package :mita.auth.admin.account)

(defstruct account id username)

(defun construct-account (&key id username)
  (make-account :id id :username username))


(defgeneric find-account-with-password-checked (conn username password))

(defgeneric find-account (conn username))

(defgeneric create-account (conn id username password))

(defgeneric delete-account (conn account-id))

(defgeneric load-accounts (conn))
