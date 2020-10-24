(defpackage :mita.auth
  (:use :cl)
  (:export :session-holder
           :lack-session-holder
           :get-session
           :renew-session-id
           :is-allowed
           :is-authenticated-p
           :authenticate))
(in-package :mita.auth)

(defvar *session-auth-key* "mita:is-authenticated-p")

(defclass session-holder () ())

(defgeneric get-session (holder))

(defgeneric renew-session-id (holder))

(defun is-authenticated-p (session-holder)
  (let ((session (get-session session-holder)))
    (gethash *session-auth-key* session)))

(defun authenticate (session-holder is-allowed-fn)
  (when (funcall is-allowed-fn)
    (let ((session (get-session session-holder)))
      (setf (gethash *session-auth-key* session) t))
    (renew-session-id session-holder)
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass lack-session-holder (session-holder)
  ((env :initarg :env)))

(defmethod get-session ((holder lack-session-holder))
  (with-slots (env) holder
    (getf env :lack.session)))

(defmethod renew-session-id ((holder lack-session-holder))
  (with-slots (env) holder
    (symbol-macrolet ((options (getf env :lack.session.options)))
      (setf (getf options :change-id) t))))
