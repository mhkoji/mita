(defpackage :mita.view
  (:use :cl)
  (:export :file
           :file-path
           :file-full-path
           :make-file
           :folder-overview
           :folder-overview-path
           :folder-overview-thumbnail-file
           :make-folder-overview
           :folder-detail
           :folder-detail-path
           :folder-detail-file-list
           :folder-detail-folder-overview-list
           :make-folder-detail))
(in-package :mita.view)

(defstruct file
  path)

(defstruct folder-overview
  path
  thumbnail-file)

(defstruct folder-detail
  path
  file-list
  folder-overview-list)
