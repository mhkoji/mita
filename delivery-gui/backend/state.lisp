(defpackage :mita.gui.state
  (:use :cl)
  (:export :viewing
           :viewing-albums
           :viewing-limit
           :viewing-next-offset
           :viewing-prev-offset
           :make-viewing))
(in-package :mita.gui.state)

(defstruct viewing
  albums
  limit
  prev-offset
  next-offset)

