(defpackage :mita.web.auth.account
  (:use :cl)
  (:export :account-authenticator))
(in-package :mita.web.auth.account)

(defclass account-authenticator (mita.web.auth:authenticator)
  ((gateway :initarg :gateway
            :reader account-authenticator-gateway)))

(defmethod mita.web.auth:is-allowed ((authenticator account-authenticator)
                                     (identifier string)
                                     (credential string))
  (let ((gw (account-authenticator-gateway authenticator)))
    (mita.account:find-account gw identifier credential)))
