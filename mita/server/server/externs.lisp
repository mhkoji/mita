(defpackage :mita.server.externs
  (:use :cl)
  (:export :*login-url*))
(in-package :mita.server.externs)

(defvar *login-url* "/auth/login")
