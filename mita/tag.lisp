(defpackage :mita.tag
  (:use :cl :mita)
  (:export :tag
           :tag-id
           :tag-name
           :make-tag
           :create-tag
           :load-tags
           :load-contents
           :tag-contents
           :update-tag-contents
           :content-tags
           :update-content-tags))
(in-package :mita.tag)

(defstruct tag id name)

(defgeneric content-id (content))

(defgeneric content-type (content))

(defgeneric load-contents (gateway type content-id-list))

(defun content->row (content)
  (mita.db:make-content
   :id (content-id content)
   :type (content-type content)))

(defun create-tag (gateway name)
  (let ((db (gateway-db gateway))
        (tag (make-tag :id (mita.id:gen) :name name)))
    (mita.db:tag-insert db tag)
    tag))

(defun load-tags (gateway)
  (mita.db:tag-select (gateway-db gateway)))

(defun tag-contents (gw tag)
  (let ((content-rows
         (mita.db:tag-content-select (gateway-db gw) (tag-id tag)))
        (type->content-id-list
         (make-hash-table :test #'equal)))
    (loop for row in content-rows
          for type = (mita.db:content-type row)
          do (progn
               (push (mita.db:content-id row)
                     (gethash type type->content-id-list))))
    (let ((id->content (make-hash-table :test #'equal)))
      (maphash (lambda (type content-id-list)
                 (dolist (content
                           (load-contents gw type content-id-list))
                   (let ((key (mita.id:to-string (content-id content))))
                     (setf (gethash key id->content) content))))
               type->content-id-list)
      (remove nil (mapcar (lambda (row)
                            (let ((key (mita.id:to-string
                                        (mita.db:content-id row))))
                              (gethash key id->content)))
                          content-rows)))))

(defun update-tag-contents (gw tag contents)
  (mita.db:tag-content-delete (gateway-db gw) (tag-id tag))
  (mita.db:tag-content-insert (gateway-db gw) (tag-id tag)
                              (mapcar #'content->row contents))
  (values))


(defun content-tags (gw content)
  (mita.db:tag-content-select-tags (gateway-db gw)
                                   (content-id content)))

(defun update-content-tags (gw content tag-ids)
  (mita.db:tag-content-insert-by-tags (gateway-db gw)
                                      tag-ids (content->row content))
  (values))
