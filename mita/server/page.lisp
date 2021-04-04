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

(defun load-page-by-id (conn page-id)
  (mita.db:page-select-by-id conn page-id))

(defun load-pages (conn)
  (mita.db:page-select conn))

(defun create-page (conn)
  (let ((id (mita.id:gen)))
    (mita.db:page-insert conn id)
    (mita.db:page-text-insert conn id "")
    (load-page-by-id conn id)))

(defun delete-page (conn page-id)
  (let ((id-list (list page-id)))
    (mita.db:page-text-delete conn id-list)
    (mita.db:page-delete conn id-list)))


(defun page-text (conn page)
  (mita.db:page-text-select conn (page-id page)))

(defun update-page-text (conn page string)
  (mita.db:page-text-update conn (page-id page) string)
  (values))


(defun page-images (conn page)
  (mita.db:page-image-select conn (page-id page)))

(defun update-page-images (conn page images)
  (mita.db:page-image-delete conn (page-id page))
  (mita.db:page-image-insert conn (page-id page) images)
  (values))
