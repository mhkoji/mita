(defpackage :mita.gui.state
  (:use :cl)
  (:export :album-list
           :album-list-albums
           :album-list-limit
           :album-list-next-offset
           :album-list-prev-offset
           :make-album-list
           :album
           :album-images
           :make-album))
(in-package :mita.gui.state)

(defstruct album-list
  albums
  limit
  prev-offset
  next-offset)

(defstruct album
  images)
