(defpackage :mita.image
  (:use :cl :mita)
  (:export :image
           :image-id
           :image-path
           :make-image
           :save-images
           :load-image
           :load-images-by-ids))
(in-package :mita.image)

(defstruct image id path)

(defun save-images (gw images)
  (mita.db:image-insert (gateway-db gw) images)
  (values))

(defun load-images-by-ids (gw image-ids)
  (mita.db:image-select-by-ids (gateway-db gw) image-ids))

(defun load-image (gw image-id)
  (car (load-images-by-ids gw (list image-id))))

