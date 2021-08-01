(defpackage :mita.tag
  (:use :cl)
  (:export :tag
           :tag-id
           :tag-name
           :tag-added-on
           :content-id
           :content-type
           :content-name
           :content-thumbnail
           :construct-tag
           :do-create-tag
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

(defstruct tag id name added-on)

(defun construct-tag (&key id name added-on)
  (make-tag :id id :name name :added-on added-on))


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


(defmacro do-create-tag ((push conn) &body body)
  (let ((g-tags (gensym)))
    `(let ((,g-tags nil))
      (labels ((,push (name &key (added-on (local-time:now)))
                 (let ((id (mita.id:gen)))
                   (let ((tag (construct-tag :id id
                                             :name name
                                             :added-on added-on)))
                     (push tag ,g-tags)))
                 (values)))
        (progn ,@body))
      (dolist (tag ,g-tags)
        (save-tag ,conn tag))
      ;; refresh entities
      (mapcar (lambda (tag)
                (load-tag-by-id ,conn (tag-id tag)))
              ,g-tags))))
