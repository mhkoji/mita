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

(defun save-images (conn images)
  (mita.db:image-insert conn images)
  (values))

(defun load-images-by-ids (conn image-ids)
  (mita.db:image-select-by-ids conn image-ids))

(defun load-image (conn image-id)
  (car (load-images-by-ids conn (list image-id))))

(defun delete-images (conn image-ids)
  (mita.db:image-delete conn image-ids))
