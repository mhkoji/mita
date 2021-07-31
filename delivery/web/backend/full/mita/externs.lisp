(defpackage :mita.delivery.web.full.server.externs
  (:use :cl)
  (:export :*login-url*))
(in-package :mita.delivery.web.full.server.externs)

(defvar *login-url* "/auth/login")
