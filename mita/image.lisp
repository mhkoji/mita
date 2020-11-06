(defpackage :mita.image
  (:use :cl :mita)
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

(defun save-images (gw images)
  (mita.db:image-insert (gateway-db gw) images)
  (values))

(defun load-images-by-ids (gw image-ids)
  (mita.db:image-select-by-ids (gateway-db gw) image-ids))

(defun load-image (gw image-id)
  (car (load-images-by-ids gw (list image-id))))

(defun delete-images (gw image-ids)
  (mita.db:image-delete (gateway-db gw) image-ids))

