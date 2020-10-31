(defpackage :mita.db
  (:use :cl)
  (:export :db
           :page-delete
           :page-insert
           :page-select-by-id
           :page-select
           :page-text-delete
           :page-text-insert
           :page-text-select
           :page-text-update
           :page-image-insert
           :page-image-select
           :page-image-delete
           :image-insert
           :image-select-by-ids
           :make-album
           :album-id
           :album-name
           :album-created-on
           :album-delete
           :album-insert
           :album-select
           :album-select-album-ids
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
           :tag-content-insert-by-tags))
(in-package :mita.db)

(defclass db () ())

(defgeneric create-tables (db))

(defgeneric page-delete (db page-id-list))

(defgeneric page-select-by-id (db page-id))

(defgeneric page-select (db))

(defgeneric page-insert (db page-id))


(defgeneric page-text-delete (db page-id-list))

(defgeneric page-text-select (db page-id))

(defgeneric page-text-update (db page-id text))

(defgeneric page-text-insert (db page-id text))


(defgeneric page-image-delete (db page-id))

(defgeneric page-image-select (db page-id))

(defgeneric page-image-insert (db page-id images))


(defgeneric image-select-by-ids (db image-id-list))

(defgeneric image-insert (db images))


(defstruct album id name created-on)

(defgeneric album-delete (db album-id-list))

(defgeneric album-select (db album-id-list))

(defgeneric album-select-album-ids (db offset limit))

(defgeneric album-insert (db albums))


(defstruct album-thumbnail-image album-id image-id)

(defgeneric album-thumbnail-image-delete (db album-id-list))

(defgeneric album-thumbnail-image-select (db album-id-list))

(defgeneric album-thumbnail-image-insert (db album-thumbnail-image-list))


(defgeneric album-image-delete (db album-id-list))

(defgeneric album-image-select (db album-id))

(defgeneric album-image-insert (db album-id images))


(defgeneric tag-delete (db tag-id-list))

(defgeneric tag-select (db))

(defgeneric tag-insert (db tag))

(defgeneric tag-update (db tag-id name))


(defstruct content id type)

(defgeneric tag-content-delete (db tag-id))

(defgeneric tag-content-delete-by-content (db content))

(defgeneric tag-content-select (db tag-id))

(defgeneric tag-content-select-tags (db content-id))

(defgeneric tag-content-insert (db tag-id contents))

(defgeneric tag-content-insert-by-tags (db tag-id-list content))
