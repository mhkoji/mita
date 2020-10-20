(defpackage :mita.id
  (:use :cl)
  (:export :id
           :gen
           :gen-from-name
           :parse
           :parse-or-nil
           :to-string
           :id=))
(in-package :mita.id)

(defclass id (uuid:uuid) ())

(defun gen ()
  (change-class (uuid:make-v4-uuid) 'id))

(defun gen-from-name (key)
  (change-class (uuid:make-v5-uuid uuid:+namespace-dns+ key) 'id))

(defun parse (str)
  (change-class (uuid:make-uuid-from-string str) 'id))

(defun parse-or-nil (str)
  (handler-case (parse str)
    (error ()
      nil)))

(defun to-string (id)
  (format nil "~A" id))

(defun id= (id1 id2)
  (uuid:uuid= id1 id2))
