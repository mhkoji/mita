(defpackage :mita.web.dep
  (:use :cl)
  (:export :dep
           :get-db
           :get-content-root
           :get-thumbnail-root))
(in-package :mita.web.dep)

;; Collection of dependencies
(defclass dep () ())

(defgeneric get-db (dep req))

(defgeneric get-content-root (dep req))

(defgeneric get-thumbnail-root (dep req))
