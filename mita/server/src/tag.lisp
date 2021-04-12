(defpackage :mita.tag
  (:use :cl)
  (:export :tag
           :tag-id
           :tag-name
           :content-id
           :content-type
           :content-name
           :content-thumbnail
           :construct-tag
           :create-tag
           :delete-tag
           :load-tags
           :load-tag-by-id
           :load-contents
           :save-tag
           :update-tag-name
           :tag-contents
           :update-tag-contents
           :content-tags
           :update-content-tags))
(in-package :mita.tag)

(defstruct tag id name)

(defun construct-tag (&key id name)
  (make-tag :id id :name name))


(defgeneric content-id (content))

(defgeneric content-type (content))

(defgeneric content-name (content))

(defgeneric content-thumbnail (content))


(defgeneric load-contents (loader type content-id-list))

(defgeneric save-tag (conn tag))

(defgeneric load-tags (conn))

(defgeneric load-tag-by-id (conn tag-id))

(defgeneric delete-tag (conn tag-id))

(defgeneric update-tag-name (conn tag name))


(defgeneric tag-contents (conn tag))

(defgeneric update-tag-contents (conn tag contents))

(defgeneric content-tags (conn content))

(defgeneric update-content-tags (conn content tag-ids))


(defun create-tag (conn name)
  (let ((id (mita.id:gen)))
    (save-tag conn (construct-tag :id id :name name))
    (load-tag-by-id conn id)))
