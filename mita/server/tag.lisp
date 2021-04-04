(defpackage :mita.tag
  (:use :cl)
  (:export :tag
           :tag-id
           :tag-name
           :content-id
           :content-type
           :content-name
           :content-thumbnail
           :make-tag
           :create-tag
           :delete-tag
           :load-tags
           :load-tag-by-id
           :load-contents
           :update-tag-name
           :tag-contents
           :update-tag-contents
           :content-tags
           :update-content-tags))
(in-package :mita.tag)

(defstruct tag id name)

(defgeneric content-id (content))

(defgeneric content-type (content))

(defgeneric content-name (content))

(defgeneric content-thumbnail (content))


(defgeneric load-contents (loader type content-id-list))

(defun content->row (content)
  (mita.db:make-content
   :id (content-id content)
   :type (content-type content)))

(defun create-tag (conn name)
  (let ((tag (make-tag :id (mita.id:gen) :name name)))
    (mita.db:tag-insert conn tag)
    tag))

(defun delete-tag (conn tag-id)
  (mita.db:tag-content-delete conn tag-id)
  (mita.db:tag-delete conn (list tag-id))
  (values))

(defun load-tags (conn)
  (mita.db:tag-select conn))

(defun load-tag-by-id (conn tag-id)
  ;; TODO: should not select all the tags
  (find tag-id (load-tags conn)
        :key #'tag-id
        :test #'mita.id:id=))

(defun tag-contents (conn tag)
  (let ((content-rows
         (mita.db:tag-content-select conn (tag-id tag)))
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
                           (load-contents conn type content-id-list))
                   (let ((key (mita.id:to-string (content-id content))))
                     (setf (gethash key id->content) content))))
               type->content-id-list)
      (remove nil (mapcar (lambda (row)
                            (let ((key (mita.id:to-string
                                        (mita.db:content-id row))))
                              (gethash key id->content)))
                          content-rows)))))

(defun update-tag-contents (conn tag contents)
  (mita.db:tag-content-delete conn (tag-id tag))
  (mita.db:tag-content-insert conn (tag-id tag)
                              (mapcar #'content->row contents))
  (values))

(defun update-tag-name (conn tag name)
  (mita.db:tag-update conn (tag-id tag) name)
  (values))

(defun content-tags (conn content)
  (mita.db:tag-content-select-tags conn (content-id content)))

(defun update-content-tags (conn content tag-ids)
  (mita.db:tag-content-delete-by-content conn (content-id content))
  ;; TODO: should not load all tags.
  (let ((tags (load-tags conn)))
    (setq tag-ids (remove-if-not (lambda (tag-id)
                                   (member tag-id tags
                                           :key #'tag-id
                                           :test #'mita.id:id=))
                                 tag-ids))
    (when tag-ids
      (let ((row (content->row content)))
        (mita.db:tag-content-insert-by-tags conn tag-ids row))))
  (values))
