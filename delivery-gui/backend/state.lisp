(defpackage :mita.gui.state
  (:use :cl)
  (:export :loading
           :make-loading
           :viewing
           :viewing-albums
           :viewing-limit
           :viewing-next-offset
           :viewing-prev-offset
           :make-viewing))
(in-package :mita.gui.state)

(defstruct loading)

(defstruct viewing
  albums
  limit
  prev-offset
  next-offset)

