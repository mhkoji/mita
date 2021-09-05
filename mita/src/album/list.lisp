(defpackage :mita.album.list
  (:use :cl)
  (:export :query
           :query-condition
           :query-sort
           :sort-item
           :sort-item-name
           :sort-item-direction
           :make-sort
           :run))
(in-package :mita.album.list)

(defclass query ()
  ((condition :initarg :condition
              :initform nil
              :reader query-condition)
   (sort :initarg :sort
         :initform nil
         :reader query-sort)))

(defstruct sort-item name direction)

(defun make-sort (name direction)
  (make-sort-item :name name :direction direction))

(defgeneric run (conn query offset limit))
