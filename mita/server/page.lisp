(defpackage :mita.page
  (:use :cl)
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

(defun load-page-by-id (db page-id)
  (mita.db:page-select-by-id db page-id))

(defun load-pages (db)
  (mita.db:page-select db))

(defun create-page (db)
  (let ((id (mita.id:gen)))
    (mita.db:page-insert db id)
    (mita.db:page-text-insert db id "")
    (load-page-by-id db id)))

(defun delete-page (db page-id)
  (let ((id-list (list page-id)))
    (mita.db:page-text-delete db id-list)
    (mita.db:page-delete db id-list)))


(defun page-text (db page)
  (mita.db:page-text-select db (page-id page)))

(defun update-page-text (db page string)
  (mita.db:page-text-update db (page-id page) string)
  (values))


(defun page-images (db page)
  (mita.db:page-image-select db (page-id page)))

(defun update-page-images (db page images)
  (mita.db:page-image-delete db (page-id page))
  (mita.db:page-image-insert db (page-id page) images)
  (values))
