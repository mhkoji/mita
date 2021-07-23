(defpackage :mita.web.externs
  (:use :cl)
  (:export :*login-url*))
(in-package :mita.web.externs)

(defvar *login-url* "/auth/login")
