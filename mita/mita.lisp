(defpackage :mita
  (:use :cl)
  (:export :gateway-db
           :gateway))
(in-package :mita)

(defgeneric gateway-db (gw))

(defclass gateway ()
  ((db
    :initarg :db
    :reader gateway-db)))
