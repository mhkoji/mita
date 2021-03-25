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

(defun create-tag (db name)
  (let ((tag (make-tag :id (mita.id:gen) :name name)))
    (mita.db:tag-insert db tag)
    tag))

(defun delete-tag (db tag-id)
  (mita.db:tag-content-delete db tag-id)
  (mita.db:tag-delete db (list tag-id))
  (values))

(defun load-tags (db)
  (mita.db:tag-select db))

(defun load-tag-by-id (db tag-id)
  ;; TODO: should not select all the tags
  (find tag-id (load-tags db)
        :key #'tag-id
        :test #'mita.id:id=))

(defun tag-contents (db tag)
  (let ((content-rows
         (mita.db:tag-content-select db (tag-id tag)))
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
                           (load-contents db type content-id-list))
                   (let ((key (mita.id:to-string (content-id content))))
                     (setf (gethash key id->content) content))))
               type->content-id-list)
      (remove nil (mapcar (lambda (row)
                            (let ((key (mita.id:to-string
                                        (mita.db:content-id row))))
                              (gethash key id->content)))
                          content-rows)))))

(defun update-tag-contents (db tag contents)
  (mita.db:tag-content-delete db (tag-id tag))
  (mita.db:tag-content-insert db (tag-id tag)
                              (mapcar #'content->row contents))
  (values))

(defun update-tag-name (db tag name)
  (mita.db:tag-update db (tag-id tag) name)
  (values))

(defun content-tags (db content)
  (mita.db:tag-content-select-tags db (content-id content)))

(defun update-content-tags (db content tag-ids)
  (mita.db:tag-content-delete-by-content db (content-id content))
  (when tag-ids
    (mita.db:tag-content-insert-by-tags db tag-ids (content->row content)))
  (values))
