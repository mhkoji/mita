(defpackage :mita.fs
  (:use :cl)
  (:export :file
           :file-name
           :file-path
           :file-created-on
           :file-size
           :folder-p
           :folder-list-children
           :make-thumbnail))
(in-package :mita.fs)

(defclass file () ())

(defgeneric file-name (file))

(defgeneric file-path (file))

(defgeneric file-created-on (file))

(defgeneric folder-p (file))

(defgeneric folder-list-children (file))

(defgeneric make-thumbnail (dir source-file))
