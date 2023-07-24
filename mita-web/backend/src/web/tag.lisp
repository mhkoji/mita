(defpackage :mita.web.tag
  (:use :cl)
  (:export :tag-id
           :tag-name
           :service
           :service-tag-store
           :service-list-folders
           :service-list-tags
           :service-tag-add
           :service-tag-folders
           :service-content-tags
           :service-content-set-tags
           :service-warmup
           :make-service))
(in-package :mita.web.tag)

(defgeneric service-tag-store (service))
(defgeneric service-list-folders (service content-id-list))

(defstruct tag id name)

(defun tag->view (tag)
  (make-tag :id (mita.tag:tag-id tag)
            :name (mita.tag:tag-name tag)))

(defun service-list-tags (service)
  (let ((tags (mita.tag:store-list-tags (service-tag-store service))))
    (mapcar #'tag->view tags)))

(defun service-tag-add (service name)
  (let ((tag (mita.tag:store-add-tag (service-tag-store service) name)))
    (tag->view tag)))

;;

(defun service-content-tags (service content)
  (let ((tags (mita.tag:content-tags (service-tag-store service) content)))
    (mapcar #'tag->view tags)))

(defun service-content-set-tags (service content tag-id-list)
  (mita.tag:content-tags-set (service-tag-store service) content tag-id-list))

(defmethod mita.tag:content-id ((c mita.file:folder))
  (namestring (mita.file:file-path c)))

(defmethod mita.tag:content-type ((c mita.file:folder))
  "folder")

(defun service-tag-folders (service tag-id)
  (let ((tag (mita.tag:store-get-tag (service-tag-store service) tag-id)))
    (let ((content-id-list (mita.tag:tag-content-id-list
                            (service-tag-store service) tag "folder")))
      (service-list-folders service content-id-list))))
