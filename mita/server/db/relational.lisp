(defpackage :mita.db.relational
  (:use :cl)
  (:export :connection
           :insert-into
           :select-from
           :delete-from
           :timestamp-to-string
           :parse-timestamp))
(in-package :mita.db.relational)

(defclass connection (mita.db:connection) ())

(defgeneric insert-into (conn table-name column-name-list values-list))

(defgeneric select-from (conn column-names table-name &key where order-by))

(defgeneric delete-from (conn table-name &key where))

(defun single (row-parser select-result)
  (car (mapcar row-parser select-result)))

(defgeneric timestamp-to-string (conn timestamp))

(defgeneric parse-timestamp (conn object))

;;;

(defclass page (mita.page:page)
  ((id
    :initarg :id
    :type mita.id:id
    :reader mita.page:page-id)
   (created-on
    :initarg :created-on
    :type local-time:timestamp
    :reader mita.page:page-created-on)))

(defun parse-page (conn row)
  (make-instance 'page
   :id (mita.id:parse (first row))
   :created-on (parse-timestamp conn (second row))))

(defmethod mita.db:page-delete ((conn connection)
                                (page-id-list list))
  (when page-id-list
    (delete-from conn "pages"
     :where `(:in "page_id" (:p ,(mapcar #'mita.id:to-string
                                         page-id-list))))))

(defmethod mita.db:page-insert ((conn connection)
                                (page-id mita.id:id))
  (let ((now (local-time:now)))
    (insert-into conn "pages" '("page_id" "created_on")
                 (list (list (mita.id:to-string page-id)
                             (timestamp-to-string conn now))))))

(defmethod mita.db:page-select-by-id ((conn connection)
                                      (page-id mita.id:id))
  (single (lambda (row)
            (parse-page conn row))
          (select-from conn "*" "pages"
           :where `(:= "page_id" (:p ,(mita.id:to-string page-id))))))
                       

(defmethod mita.db:page-select ((conn connection))
  (mapcar (lambda (row)
            (parse-page conn row))
          (select-from conn "*" "pages")))


(defmethod mita.db:page-text-delete ((conn connection)
                                     (page-id-list list))
  (when page-id-list
    (delete-from conn "page_text"
     :where `(:in "page_id" (:p ,(mapcar #'mita.id:to-string
                                         page-id-list))))))
                       

(defmethod mita.db:page-text-insert ((conn connection)
                                     (page-id mita.id:id)
                                     (text string))
  (insert-into conn "page_text" '("page_id" "string")
               (list (list (mita.id:to-string page-id) text))))

(defmethod mita.db:page-text-select ((conn connection)
                                     (page-id mita.id:id))
  (single #'first
          (select-from conn "string" "page_text"
           :where `(:= "page_id" (:p ,(mita.id:to-string page-id))))))

(defun parse-image (row)
  (mita.image:make-image
   :id (mita.id:parse (first row))
   :source (alexandria:make-keyword (second row))
   :path (third row)))

(defmethod mita.db:image-select-by-ids ((conn connection)
                                        (image-id-list list))
  (mapcar #'parse-image
          (select-from conn "*" "images"
           :where `(:in "image_id" (:p ,(mapcar #'mita.id:to-string
                                                image-id-list))))))

(defmethod mita.db:image-insert ((conn connection)
                                 (images list))
  (insert-into conn "images" '("image_id" "source" "path")
               (mapcar
                (lambda (image)
                  (list (mita.id:to-string (mita.image:image-id image))
                        (symbol-name (mita.image:image-source image))
                        (mita.image:image-path image)))
                images)))

(defmethod mita.db:image-delete ((conn connection)
                                 (image-id-list list))
  (when image-id-list
    (delete-from conn "images"
     :where `(:in "image_id" (:p ,(mapcar #'mita.id:to-string
                                          image-id-list))))))
                       

(defmethod mita.db:page-image-insert ((conn connection)
                                      (page-id mita.id:id)
                                      (images list))
  (insert-into conn "page_image" '("page_id" "image_id")
               (mapcar
                (lambda (image)
                  (list (mita.id:to-string page-id)
                        (mita.id:to-string (mita.image:image-id image))))
                images)))

(defmethod mita.db:page-image-delete ((conn connection)
                                      (page-id mita.id:id))
  (delete-from conn "page_image"
               :where `(:= "page_id" (:p ,(mita.id:to-string page-id)))))
               

(defmethod mita.db:page-image-select ((conn connection)
                                      (page-id mita.id:id))
  (mapcar #'parse-image
          (select-from conn
                       "i.image_id, i.path"
                       "images AS i
                          INNER JOIN page_image
                          ON
                            i.image_id = page_image.image_id"
           :where `(:= "page_image.page_id"
                       (:p ,(mita.id:to-string page-id))))))


(defmethod mita.db:album-delete ((conn connection)
                                 (album-id-list list))
  (when album-id-list
    (delete-from conn "albums"
     :where `(:in "album_id" (:p ,(mapcar #'mita.id:to-string
                                          album-id-list))))))

(defmethod mita.db:album-insert ((conn connection)
                                 (albums list))
  (insert-into conn "albums" '("album_id" "name" "created_on")
               (mapcar
                (lambda (album)
                  (list (mita.id:to-string (mita.db:album-id album))
                        (mita.db:album-name album)
                        (timestamp-to-string
                         conn (mita.db:album-created-on album))))
                albums)))

(defmethod mita.db:album-select ((conn connection)
                                 (album-id-list list))
  (when album-id-list
    (mapcar (lambda (row)
              (mita.db:make-album
               :id (mita.id:parse (first row))
               :name (second row)
               :created-on (parse-timestamp conn (third row))))
            (select-from conn "*" "albums"
             :where `(:in "album_id" (:p ,(mapcar #'mita.id:to-string
                                                  album-id-list)))))))

(defmethod mita.db:album-thumbnail-image-delete ((conn connection)
                                                 (album-id-list list))
  (when album-id-list
    (delete-from conn "album_thumbnail_image"
     :where `(:in "album_id" (:p ,(mapcar #'mita.id:to-string
                                          album-id-list))))))

(defmethod mita.db:album-thumbnail-image-select ((conn connection)
                                                 (album-id-list list))
  (when album-id-list
    (mapcar (lambda (row)
              (mita.db:make-album-thumbnail-image
               :album-id (mita.id:parse (first row))
               :image-id (mita.id:parse (second row))))
            (select-from conn "*" "album_thumbnail_image"
             :where `(:in "album_id" (:p ,(mapcar #'mita.id:to-string
                                                  album-id-list)))))))

(defmethod mita.db:album-thumbnail-image-insert ((conn connection)
                                                 (rows list))
  (insert-into conn "album_thumbnail_image" '("album_id" "image_id")
               (mapcar
                (lambda (row)
                  (list (mita.id:to-string
                         (mita.db:album-thumbnail-image-album-id row))
                        (mita.id:to-string
                         (mita.db:album-thumbnail-image-image-id row))))
                rows)))


(defmethod mita.db:album-image-insert ((conn connection)
                                       (album-id mita.id:id)
                                       (images list))
  (insert-into conn "album_image" '("album_id" "image_id")
               (mapcar
                (lambda (image)
                  (list (mita.id:to-string album-id)
                        (mita.id:to-string (mita.image:image-id image))))
                images)))

(defmethod mita.db:album-image-delete ((conn connection)
                                       (album-id-list list))
  (when album-id-list
    (delete-from conn "album_image"
     :where `(:in "album_id"
                  (:p ,(mapcar #'mita.id:to-string album-id-list))))))

(defmethod mita.db:album-image-select ((conn connection)
                                       (album-id mita.id:id))
  (mapcar #'parse-image
          (select-from conn
                       "i.image_id, i.path"
                       "images AS i
                          INNER JOIN album_image
                          ON
                            i.image_id = album_image.image_id"
           :where `(:in "album_image.album_id"
                        (:p ,(mita.id:to-string album-id))))))


(defun parse-tag (row)
  (mita.tag:make-tag
   :id (mita.id:parse (first row))
   :name (second row)))


(defmethod mita.db:tag-delete ((conn connection)
                               (tag-id-list list))
  (when tag-id-list
    (delete-from conn "tags"
     :where `(:in "tag_id" (:p ,(mapcar #'mita.id:to-string tag-id-list))))))
                  

(defmethod mita.db:tag-select ((conn connection))
  (mapcar #'parse-tag
          (select-from conn "*" "tags" :order-by "added_on")))

(defmethod mita.db:tag-insert ((conn connection)
                               (tag mita.tag:tag))
  (insert-into conn "tags" '("tag_id" "name" "added_on")
               (list
                (list (mita.id:to-string (mita.tag:tag-id tag))
                      (mita.tag:tag-name tag)
                      (timestamp-to-string conn (local-time:now))))))

(defmethod mita.db:tag-content-delete ((conn connection)
                                       (tag-id mita.id:id))
  (delete-from conn "tag_content"
   :where `(:= "tag_id" (:p ,(mita.id:to-string tag-id)))))

(defmethod mita.db:tag-content-delete-by-content ((conn connection)
                                                  (content-id mita.id:id))
  (delete-from conn "tag_content"
   :where `(:= "content_id" (:p ,(mita.id:to-string content-id)))))

(defmethod mita.db:tag-content-select ((conn connection)
                                       (tag-id mita.id:id))
  (mapcar (lambda (row)
            (mita.db:make-content
             :id (mita.id:parse (first row))
             :type (alexandria:make-keyword (second row))))
          (select-from conn "content_id, content_type" "tag_content"
           :where `(:= "tag_id" (:p ,(mita.id:to-string tag-id)))
           :order-by "added_on")))

(defmethod mita.db:tag-content-select-tags ((conn connection)
                                            (content-id mita.id:id))
  (mapcar #'parse-tag
          (select-from conn
                       "t.tag_id, t.name"
                       "tags AS t
                          INNER JOIN tag_content
                          ON
                            t.tag_id = tag_content.tag_id"
           :where `(:= "content_id" (:p ,(mita.id:to-string content-id))))))
                       

(defmethod mita.db:tag-content-insert ((conn connection)
                                       (tag-id mita.id:id)
                                       (contents list))
  (let ((added-on-string (timestamp-to-string conn (local-time:now))))
    (insert-into conn "tag_content"
                 '("tag_id" "content_type" "content_id" "added_on")
                 (mapcar
                  (lambda (content)
                    (list (mita.id:to-string tag-id)
                          (string (mita.db:content-type content))
                          (mita.id:to-string (mita.db:content-id content))
                          added-on-string))
                  contents))))

(defmethod mita.db:tag-content-insert-by-tags ((conn connection)
                                               (tag-id-list list)
                                               (content mita.db:content))
  (let ((added-on-string (timestamp-to-string conn (local-time:now))))
    (insert-into conn "tag_content"
                 '("tag_id" "content_type" "content_id" "added_on")
                 (mapcar
                  (lambda (tag-id)
                    (list (mita.id:to-string tag-id)
                          (string (mita.db:content-type content))
                          (mita.id:to-string (mita.db:content-id content))
                          added-on-string))
                  tag-id-list))))
