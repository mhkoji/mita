(defpackage :mita.db.rdb.common
  (:use :cl)
  (:import-from :mita.db.rdb
                :connection)
  (:export :insert-into
           :select-from
           :delete-from
           :timestamp-to-string
           :parse-timestamp))
(in-package :mita.db.rdb.common)

(defgeneric insert-into (conn table-name column-name-list values-list))

(defgeneric select-from (conn column-names table-name &key where order-by))

(defgeneric delete-from (conn table-name &key where))

(defun single (row-parser select-result)
  (car (mapcar row-parser select-result)))

(defgeneric timestamp-to-string (conn timestamp))

(defgeneric parse-timestamp (conn object))

;;;

(defun parse-image (row)
  (mita.image:make-image
   :id (mita.id:parse (first row))
   :source (alexandria:make-keyword (second row))
   :path (third row)))

(defmethod mita.db.rdb:image-select-by-ids ((conn connection)
                                            (image-id-list list))
  (mapcar #'parse-image
          (select-from conn "*" "images"
           :where `(:in "image_id" (:p ,(mapcar #'mita.id:to-string
                                                image-id-list))))))

(defmethod mita.db.rdb:image-insert ((conn connection)
                                     (images list))
  (insert-into conn "images" '("image_id" "source" "path")
               (mapcar
                (lambda (image)
                  (list (mita.id:to-string (mita.image:image-id image))
                        (symbol-name (mita.image:image-source image))
                        (mita.image:image-path image)))
                images)))

(defmethod mita.db.rdb:image-delete ((conn connection)
                                     (image-id-list list))
  (when image-id-list
    (delete-from conn "images"
     :where `(:in "image_id" (:p ,(mapcar #'mita.id:to-string
                                          image-id-list))))))
                       

(defmethod mita.db.rdb:album-delete ((conn connection)
                                     (album-id-list list))
  (when album-id-list
    (delete-from conn "albums"
     :where `(:in "album_id" (:p ,(mapcar #'mita.id:to-string
                                          album-id-list))))))

(defmethod mita.db.rdb:album-insert ((conn connection)
                                     (albums list))
  (insert-into conn "albums" '("album_id" "name" "created_on")
               (mapcar
                (lambda (album)
                  (list (mita.id:to-string (mita.db.rdb:album-id album))
                        (mita.db.rdb:album-name album)
                        (timestamp-to-string
                         conn (mita.db.rdb:album-created-on album))))
                albums)))

(defmethod mita.db.rdb:album-select ((conn connection)
                                  (album-id-list list))
  (when album-id-list
    (mapcar (lambda (row)
              (mita.db.rdb:make-album
               :id (mita.id:parse (first row))
               :name (second row)
               :created-on (parse-timestamp conn (third row))))
            (select-from conn "album_id, name, created_on" "albums"
             :where `(:in "album_id" (:p ,(mapcar #'mita.id:to-string
                                                  album-id-list)))))))

(defmethod mita.db.rdb:album-thumbnail-image-delete ((conn connection)
                                                     (album-id-list list))
  (when album-id-list
    (delete-from conn "album_thumbnail_image"
     :where `(:in "album_id" (:p ,(mapcar #'mita.id:to-string
                                          album-id-list))))))

(defmethod mita.db.rdb:album-thumbnail-image-select ((conn connection)
                                                     (album-id-list list))
  (when album-id-list
    (mapcar (lambda (row)
              (mita.db.rdb:make-album-thumbnail-image
               :album-id (mita.id:parse (first row))
               :image-id (mita.id:parse (second row))))
            (select-from conn "*" "album_thumbnail_image"
             :where `(:in "album_id" (:p ,(mapcar #'mita.id:to-string
                                                  album-id-list)))))))

(defmethod mita.db.rdb:album-thumbnail-image-insert ((conn connection)
                                                  (rows list))
  (insert-into conn "album_thumbnail_image" '("album_id" "image_id")
               (mapcar
                (lambda (row)
                  (list (mita.id:to-string
                         (mita.db.rdb:album-thumbnail-image-album-id row))
                        (mita.id:to-string
                         (mita.db.rdb:album-thumbnail-image-image-id row))))
                rows)))


(defmethod mita.db.rdb:album-image-insert ((conn connection)
                                           (album-id mita.id:id)
                                           (images list))
  (insert-into conn "album_image" '("album_id" "image_id")
               (mapcar
                (lambda (image)
                  (list (mita.id:to-string album-id)
                        (mita.id:to-string (mita.image:image-id image))))
                images)))

(defmethod mita.db.rdb:album-image-delete ((conn connection)
                                           (album-id-list list))
  (when album-id-list
    (delete-from conn "album_image"
     :where `(:in "album_id"
                  (:p ,(mapcar #'mita.id:to-string album-id-list))))))

(defmethod mita.db.rdb:album-image-select ((conn connection)
                                           (album-id mita.id:id))
  (mapcar #'parse-image
          (select-from conn
                       "i.image_id, i.source, i.path"
                       "images AS i
                          INNER JOIN album_image
                          ON
                            i.image_id = album_image.image_id"
           :where `(:in "album_image.album_id"
                        (:p ,(mita.id:to-string album-id))))))


(defun parse-tag (conn row)
  (mita.tag:construct-tag
   :id (mita.id:parse (first row))
   :name (second row)
   :added-on (parse-timestamp conn (third row))))


(defmethod mita.db.rdb:tag-delete ((conn connection)
                                (tag-id-list list))
  (when tag-id-list
    (delete-from conn "tags"
     :where `(:in "tag_id" (:p ,(mapcar #'mita.id:to-string tag-id-list))))))
                  

(defmethod mita.db.rdb:tag-select ((conn connection))
  (mapcar (lambda (row)
            (parse-tag conn row))
          (select-from conn "*" "tags" :order-by "added_on")))

(defmethod mita.db.rdb:tag-insert ((conn connection)
                                   (tag mita.tag:tag))
  (insert-into conn "tags" '("tag_id" "name" "added_on")
               (list
                (list (mita.id:to-string (mita.tag:tag-id tag))
                      (mita.tag:tag-name tag)
                      (timestamp-to-string conn (mita.tag:tag-added-on tag))))))

(defmethod mita.db.rdb:tag-content-delete ((conn connection)
                                           (tag-id mita.id:id))
  (delete-from conn "tag_content"
   :where `(:= "tag_id" (:p ,(mita.id:to-string tag-id)))))

(defmethod mita.db.rdb:tag-content-delete-by-content ((conn connection)
                                                      (content-id mita.id:id))
  (delete-from conn "tag_content"
   :where `(:= "content_id" (:p ,(mita.id:to-string content-id)))))

(defmethod mita.db.rdb:tag-content-select ((conn connection)
                                           (tag-id mita.id:id))
  (mapcar (lambda (row)
            (mita.db.rdb:make-content
             :id (mita.id:parse (first row))
             :type (alexandria:make-keyword (second row))))
          (select-from conn "content_id, content_type" "tag_content"
           :where `(:= "tag_id" (:p ,(mita.id:to-string tag-id)))
           :order-by "added_on")))

(defmethod mita.db.rdb:tag-content-select-tags ((conn connection)
                                                (content-id mita.id:id))
  (mapcar (lambda (row)
            (parse-tag conn row))
          (select-from conn
                       "t.tag_id, t.name, t.added_on"
                       "tags AS t
                          INNER JOIN tag_content
                          ON
                            t.tag_id = tag_content.tag_id"
           :where `(:= "content_id" (:p ,(mita.id:to-string content-id))))))
                       

(defmethod mita.db.rdb:tag-content-insert ((conn connection)
                                           (tag-id mita.id:id)
                                           (contents list))
  (let ((added-on-string (timestamp-to-string conn (local-time:now))))
    (insert-into conn "tag_content"
                 '("tag_id" "content_type" "content_id" "added_on")
                 (mapcar
                  (lambda (content)
                    (list (mita.id:to-string tag-id)
                          (string (mita.db.rdb:content-type content))
                          (mita.id:to-string
                           (mita.db.rdb:content-id content))
                          added-on-string))
                  contents))))

(defmethod mita.db.rdb:tag-content-insert-by-tags
    ((conn connection)
     (tag-id-list list)
     (content mita.db.rdb:content))
  (let ((added-on-string (timestamp-to-string conn (local-time:now))))
    (insert-into conn "tag_content"
                 '("tag_id" "content_type" "content_id" "added_on")
                 (mapcar
                  (lambda (tag-id)
                    (list (mita.id:to-string tag-id)
                          (string (mita.db.rdb:content-type content))
                          (mita.id:to-string
                           (mita.db.rdb:content-id content))
                          added-on-string))
                  tag-id-list))))
