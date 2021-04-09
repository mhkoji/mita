(in-package :mita.rdb)

(defun content->row (content)
  (make-content
   :id (mita.tag:content-id content)
   :type (mita.tag:content-type content)))

(defmethod mita.tag:create-tag ((conn mita.rdb:connection)
                                (name string))
  (let ((id (mita.id:gen)))
    (tag-insert conn (mita.tag:construct-tag :id id :name name))
    (mita.tag:load-tag-by-id conn id)))

(defmethod mita.tag:delete-tag ((conn mita.rdb:connection)
                                (tag-id mita.id:id))
  (tag-content-delete conn tag-id)
  (tag-delete conn (list tag-id))
  (values))

(defmethod mita.tag:load-tags ((conn mita.rdb:connection))
  (tag-select conn))

(defmethod mita.tag:load-tag-by-id ((conn mita.rdb:connection)
                                    (tag-id mita.id:id))
  ;; TODO: should not select all the tags
  (find tag-id (mita.tag:load-tags conn)
        :key #'mita.tag:tag-id
        :test #'mita.id:id=))

(defmethod mita.tag:tag-contents ((conn mita.rdb:connection)
                                  (tag mita.tag:tag))
  (let ((content-rows
         (tag-content-select conn (mita.tag:tag-id tag)))
        (type->content-id-list
         (make-hash-table :test #'equal)))
    (loop for row in content-rows
          for type = (content-type row)
          do (progn
               (push (content-id row)
                     (gethash type type->content-id-list))))
    (let ((id->content (make-hash-table :test #'equal)))
      (maphash (lambda (type content-id-list)
                 (dolist (content
                           (mita.tag:load-contents conn
                                                   type
                                                   content-id-list))
                   (let ((key (mita.id:to-string
                               (mita.tag:content-id content))))
                     (setf (gethash key id->content) content))))
               type->content-id-list)
      (remove nil (mapcar (lambda (row)
                            (let ((key (mita.id:to-string (content-id row))))
                              (gethash key id->content)))
                          content-rows)))))

(defmethod mita.tag:update-tag-contents ((conn mita.rdb:connection)
                                         (tag mita.tag:tag)
                                         (contents list))
  (let ((tag-id (mita.tag:tag-id tag)))
    (tag-content-delete conn tag-id)
    (tag-content-insert conn tag-id (mapcar #'content->row contents)))
  (values))

(defmethod mita.tag:update-tag-name ((conn mita.rdb:connection)
                                     (tag mita.tag:tag)
                                     (name string))
  (tag-update conn (mita.tag:tag-id tag) name)
  (values))

(defmethod mita.tag:content-tags ((conn mita.rdb:connection) content)
  (tag-content-select-tags conn (mita.tag:content-id content)))

(defmethod mita.tag:update-content-tags ((conn mita.rdb:connection)
                                         (content t)
                                         (tag-ids list))
  (tag-content-delete-by-content conn (mita.tag:content-id content))
  ;; TODO: should not load all tags.
  (let ((tags (mita.tag:load-tags conn)))
    (setq tag-ids (remove-if-not (lambda (tag-id)
                                   (member tag-id tags
                                           :key #'mita.tag:tag-id
                                           :test #'mita.id:id=))
                                 tag-ids))
    (when tag-ids
      (tag-content-insert-by-tags conn tag-ids (content->row content))))
  (values))
