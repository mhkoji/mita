(defpackage :mita.file
  (:use :cl)
  (:export :repository
           :file
           :file-name
           :file-path
           :file-created-on
           :file-size
           :folder-p
           :folder-list-children
           :make-thumbnail))
(in-package :mita.file)

(defclass repository () ())

(defclass file () ())

(defgeneric file-name (file))

(defgeneric file-path (file))

(defgeneric file-created-on (file))

(defgeneric folder-p (file))


(defgeneric folder-list-children (repos file))

(defgeneric make-thumbnail (thumbnail-repos source-file))
