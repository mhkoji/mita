(defpackage :mita.page
  (:use :cl :mita)
  (:export :page
           :page-id
           :page-created-on
           :delete-page
           :create-page
           :load-page-by-id
           :load-pages
           :page-text
           :update-page-text
           :page-images
           :update-page-images))
(in-package :mita.page)

(defclass page () ())

(defgeneric page-id (page))

(defgeneric page-created-on (page))

(defun load-page-by-id (gateway page-id)
  (mita.db:page-select-by-id (gateway-db gateway) page-id))

(defun load-pages (gateway)
  (mita.db:page-select (gateway-db gateway)))

(defun create-page (gateway)
  (let ((db (gateway-db gateway))
        (id (mita.id:gen)))
    (mita.db:page-insert db id)
    (mita.db:page-text-insert db id "")
    (load-page-by-id gateway id)))

(defun delete-page (gateway page-id)
  (let ((db (gateway-db gateway))
        (id-list (list page-id)))
    (mita.db:page-text-delete db id-list)
    (mita.db:page-delete db id-list)))


(defun page-text (gateway page)
  (mita.db:page-text-select (gateway-db gateway) (page-id page)))

(defun update-page-text (gateway page string)
  (let ((db (gateway-db gateway)))
    (mita.db:page-text-update db (page-id page) string))
  (values))


(defun page-images (gateway page)
  (let ((db (gateway-db gateway)))
    (mita.db:page-image-select db (page-id page))))

(defun update-page-images (gateway page images)
  (let ((db (gateway-db gateway)))
    (mita.db:page-image-delete db (page-id page))
    (mita.db:page-image-insert db (page-id page) images))
  (values))
