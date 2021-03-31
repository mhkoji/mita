(defpackage :mita.fs
  (:use :cl)
  (:export :file
           :file-name
           :file-path
           :file-created-on
           :file-size
           :dir-p
           :dir-list-children
           :make-thumbnail))
(in-package :mita.fs)

(defclass file () ())

(defgeneric file-name (file))

(defgeneric file-path (file))

(defgeneric file-created-on (file))

(defgeneric dir-p (file))

(defgeneric dir-list-children (file))

(defgeneric make-thumbnail (dir source-file))
