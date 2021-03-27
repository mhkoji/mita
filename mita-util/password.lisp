(defpackage :mita.util.password
  (:use :cl)
  (:export :make-hashed-password
           :hash
           :hashed-password-string
           :hashed-password-matches-p))
(in-package :mita.util.password)

(defstruct hashed-password string)

(defun hash (raw)
  (make-hashed-password
   :string
   (cl-bcrypt:encode
    (cl-bcrypt:make-password raw))))

(defun hashed-password-matches-p (hashed password)
  (cl-bcrypt:password= password (hashed-password-string hashed)))
