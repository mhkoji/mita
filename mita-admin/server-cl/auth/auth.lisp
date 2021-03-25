(defpackage :mita.auth
  (:use :cl)
  (:export :session-holder
           :get-session
           :renew-session-id
           :is-authenticated-p
           :authenticate
           :*session-account-id-key*)
  (:import-from :alexandria
                :when-let
                :when-let*))
(in-package :mita.auth)

(defvar *session-account-id-key* "mita.auth:account-id")


(defclass session-holder () ())

(defgeneric get-session (holder))

(defgeneric renew-session-id (holder))

(defmacro with-admin-db ((db connector) &body body)
  `(mita.db.impl:with-admin-db (,db ,connector)
     ,@body))

(defun is-authenticated-p (session-holder connector)
  (let ((session (get-session session-holder)))
    (when-let* ((id-str (gethash *session-account-id-key* session))
                (account-id (mita.id:parse-or-nil id-str)))
      (with-admin-db (db connector)
        (mita.account:find-account-by-id db account-id)))))

(defun find-account (connector username password)
  (when (and username password)
    (with-admin-db (db connector)
      (mita.account:find-account-with-password-checked
       db username password))))

(defun authenticate (session-holder connector username password)
  (when-let ((account (find-account connector username password)))
    (let ((session (get-session session-holder)))
      (setf (gethash *session-account-id-key* session)
            (mita.id:to-string (mita.account:account-id account)))
      (renew-session-id session-holder)
      t)))
