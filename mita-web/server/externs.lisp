(defpackage :mita.web.server.externs
  (:use :cl)
  (:export :*login-url*))
(in-package :mita.web.server.externs)

(defvar *login-url* "/auth/login")
