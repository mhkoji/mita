(defpackage :mita.db.rdb
  (:use :cl)
  (:export :connection
           :image-insert
           :image-select-by-ids
           :image-delete
           :make-album
           :album-id
           :album-name
           :album-created-on
           :album-delete
           :album-insert
           :album-select
           :album-select-album-ids
           :album-select-album-ids-by-query
           :make-album-thumbnail-image
           :album-thumbnail-image-album-id
           :album-thumbnail-image-image-id
           :album-thumbnail-image-delete
           :album-thumbnail-image-insert
           :album-thumbnail-image-select
           :album-image-delete
           :album-image-insert
           :album-image-select
           :tag-delete
           :tag-select
           :tag-insert
           :tag-update
           :make-content
           :content
           :content-id
           :content-type
           :tag-content-delete
           :tag-content-delete-by-content
           :tag-content-select
           :tag-content-select-tags
           :tag-content-insert
           :tag-content-insert-by-tags)
  (:import-from :alexandria
                :when-let))
(in-package :mita.db.rdb)

(defclass connection (mita.db:connection) ())


(defgeneric image-select-by-ids (conn image-id-list))

(defgeneric image-insert (conn images))

(defgeneric image-delete (conn image-ids))


(defstruct album id name created-on)

(defgeneric album-delete (conn album-id-list))

(defgeneric album-select (conn album-id-list))

(defgeneric album-select-album-ids (conn offset limit))

(defgeneric album-select-album-ids-by-query (conn query offset limit))

(defgeneric album-insert (conn albums))


(defstruct album-thumbnail-image album-id image-id)

(defgeneric album-thumbnail-image-delete (conn album-id-list))

(defgeneric album-thumbnail-image-select (conn album-id-list))

(defgeneric album-thumbnail-image-insert (conn album-thumbnail-image-list))


(defgeneric album-image-delete (conn album-id-list))

(defgeneric album-image-select (conn album-id))

(defgeneric album-image-insert (conn album-id images))


(defgeneric tag-delete (conn tag-id-list))

(defgeneric tag-select (conn))

(defgeneric tag-insert (conn tag))

(defgeneric tag-update (conn tag-id name))


(defstruct content id type)

(defgeneric tag-content-delete (conn tag-id))

(defgeneric tag-content-delete-by-content (conn content))

(defgeneric tag-content-select (conn tag-id))

(defgeneric tag-content-select-tags (conn content-id))

(defgeneric tag-content-insert (conn tag-id contents))

(defgeneric tag-content-insert-by-tags (conn tag-id-list content))
