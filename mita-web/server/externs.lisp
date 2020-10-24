(defpackage :mita.web.server.externs
  (:use :cl)
  (:export :*login-url*
           :*session-auth-key*))
(in-package :mita.web.server.externs)

(defvar *login-url* "/auth/login")

(defvar *session-auth-key* "mita:is-authenticated-p")
