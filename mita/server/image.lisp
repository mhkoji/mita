(defpackage :mita.image
  (:use :cl)
  (:export :image
           :image-id
           :image-source
           :image-path
           :make-image
           :save-images
           :load-image
           :load-images-by-ids
           :delete-images
           :+source-content+
           :+source-thumbnail+))
(in-package :mita.image)

(defvar +source-content+ :content)

(defvar +source-thumbnail+ :thumbnail)

(defstruct image id source path)

(defun save-images (db images)
  (mita.db:image-insert db images)
  (values))

(defun load-images-by-ids (db image-ids)
  (mita.db:image-select-by-ids db image-ids))

(defun load-image (db image-id)
  (car (load-images-by-ids db (list image-id))))

(defun delete-images (db image-ids)
  (mita.db:image-delete db image-ids))
