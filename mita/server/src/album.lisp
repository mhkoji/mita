(defpackage :mita.album
  (:use :cl)
  (:export :album
           :album-id
           :album-name
           :album-thumbnail
           :construct-album

           :album-source-id
           :album-source-name
           :album-source-thumbnail
           :album-source-created-on
           :create-albums
           :load-album-by-id
           :load-albums-in
           :load-albums
           :delete-albums

           :album-images
           :update-album-images
           :make-album-source

           :create-with-images
           :delete-with-imagesn)
  (:import-from :alexandria :when-let))
(in-package :mita.album)

(defstruct album id name thumbnail)

(defun construct-album (&key id name thumbnail)
  (make-album :id id :name name :thumbnail thumbnail))


(defstruct album-source id name created-on thumbnail)

(defgeneric create-albums (conn album-sources))

(defgeneric load-albums-in (conn album-id-list))

(defgeneric load-albums (conn offset limit))

(defgeneric delete-albums (conn album-ids))

(defun load-album-by-id (conn album-id)
  (car (load-albums-in conn (list album-id))))


(defgeneric album-images (conn album))

(defgeneric update-album-images (conn album images))
