(defpackage :mita.account
  (:use :cl)
  (:export :account
           :account-id
           :find-account
           :create-account)
  (:import-from :alexandria
                :when-let))
(in-package :mita.account)

(defclass account ()
  ((row :initarg :row)))

(defun account-id (a)
  (mita.account.db:account-id (slot-value a 'row)))

(defun find-account (gw username password)
  (let ((db (mita:gateway-db gw))
        (hashed (mita.account.db:hash-password password)))
    (when-let ((row (mita.account.db:account-select db username hashed)))
      (make-instance 'account :row row))))

(defun create-account (gw username password)
  (mita.account.db:account-insert
   (mita:gateway-db gw)
   (mita.account.db:make-account :id (mita.id:gen)
                                 :username username)
   (mita.account.db:hash-password password)))
