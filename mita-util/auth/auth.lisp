(defpackage :mita.util.auth
  (:use :cl)
  (:export :session-holder
           :get-session
           :renew-session-id
           :is-authenticated-p
           :authenticate
           :account-identity
           :find-account
           :find-account-from-unique-string
           :*session-account-id-key*)
  (:import-from :alexandria
                :when-let))
(in-package :mita.util.auth)

(defvar *session-account-id-key* "mita.util.auth:account-id")

(defgeneric account-identity (account))

(defgeneric find-account (account-repository username password))

(defclass session-holder () ())

(defgeneric get-session (holder))

(defgeneric renew-session-id (holder))

(defun is-authenticated-p (session-holder)
  (let ((session (get-session session-holder)))
    (gethash *session-account-id-key* session)))

(defun authenticate (session-holder repos username password)
  (when-let ((account (find-account repos username password)))
    (let ((session (get-session session-holder)))
      (setf (gethash *session-account-id-key* session)
            (account-identity account))
      (renew-session-id session-holder)
      t)))
