(defpackage :mita.db.relational
  (:use :cl)
  (:export :rdb
           :insert-into
           :select-from
           :delete-from))
(in-package :mita.db.relational)

(defclass rdb (mita.db:db) ())

(defgeneric insert-into (rdb table-name column-name-list values-list))

(defgeneric select-from (rdb column-names table-name &key where order-by))

(defgeneric delete-from (rdb table-name &key where))

(defun single (row-parser select-result)
  (car (mapcar row-parser select-result)))

(defclass page (mita.page:page)
  ((id
    :initarg :id
    :type mita.id:id
    :reader mita.page:page-id)
   (created-on
    :initarg :created-on
    :type local-time:timestamp
    :reader mita.page:page-created-on)))

(defun parse-page (row)
  (make-instance 'page
   :id (mita.id:parse (first row))
   :created-on (local-time:universal-to-timestamp (second row))))

(defmethod mita.db:page-delete ((db rdb)
                                (page-id-list list))
  (when page-id-list
    (delete-from db "pages"
     :where `(:in "page_id" (:p ,(mapcar #'mita.id:to-string
                                         page-id-list))))))

(defmethod mita.db:page-insert ((db rdb)
                                (page-id mita.id:id))
  (let ((now (local-time:now)))
    (insert-into db "pages" '("page_id" "created_on")
                 (list (list (mita.id:to-string page-id)
                             (local-time:to-rfc3339-timestring now))))))

(defmethod mita.db:page-select-by-id ((db rdb)
                                      (page-id mita.id:id))
  (single #'parse-page
          (select-from db "*" "pages"
           :where `(:= "page_id" (:p ,(mita.id:to-string page-id))))))
                       

(defmethod mita.db:page-select ((db rdb))
  (mapcar #'parse-page (select-from db "*" "pages")))


(defmethod mita.db:page-text-delete ((db rdb)
                                     (page-id-list list))
  (when page-id-list
    (delete-from db "page_text"
     :where `(:in "page_id" (:p ,(mapcar #'mita.id:to-string
                                         page-id-list))))))
                       

(defmethod mita.db:page-text-insert ((db rdb)
                                     (page-id mita.id:id)
                                     (text string))
  (insert-into db "page_text" '("page_id" "string")
               (list (list (mita.id:to-string page-id) text))))

(defmethod mita.db:page-text-select ((db rdb)
                                     (page-id mita.id:id))
  (single #'first
          (select-from db "string" "page_text"
           :where `(:= "page_id" (:p ,(mita.id:to-string page-id))))))

(defun parse-image (row)
  (mita.image:make-image
   :id (mita.id:parse (first row))
   :source (alexandria:make-keyword (second row))
   :path (third row)))

(defmethod mita.db:image-select-by-ids ((db rdb)
                                        (image-id-list list))
  (mapcar #'parse-image
          (select-from db "*" "images"
           :where `(:in "image_id" (:p ,(mapcar #'mita.id:to-string
                                                image-id-list))))))

(defmethod mita.db:image-insert ((db rdb)
                                 (images list))
  (insert-into db "images" '("image_id" "source" "path")
               (mapcar
                (lambda (image)
                  (list (mita.id:to-string (mita.image:image-id image))
                        (symbol-name (mita.image:image-source image))
                        (mita.image:image-path image)))
                images)))

(defmethod mita.db:image-delete ((db rdb)
                                 (image-id-list list))
  (when image-id-list
    (delete-from db "images"
     :where `(:in "image_id" (:p ,(mapcar #'mita.id:to-string
                                          image-id-list))))))
                       

(defmethod mita.db:page-image-insert ((db rdb)
                                      (page-id mita.id:id)
                                      (images list))
  (insert-into db "page_image" '("page_id" "image_id")
               (mapcar
                (lambda (image)
                  (list (mita.id:to-string page-id)
                        (mita.id:to-string (mita.image:image-id image))))
                images)))

(defmethod mita.db:page-image-delete ((db rdb)
                                      (page-id mita.id:id))
  (delete-from db "page_image"
               :where `(:= "page_id" (:p ,(mita.id:to-string page-id)))))
               

(defmethod mita.db:page-image-select ((db rdb)
                                      (page-id mita.id:id))
  (mapcar #'parse-image
          (select-from db
                       "i.image_id, i.path"
                       "images AS i
                          INNER JOIN page_image
                          ON
                            i.image_id = page_image.image_id"
           :where `(:= "page_image.page_id"
                       (:p ,(mita.id:to-string page-id))))))


(defmethod mita.db:album-delete ((db rdb)
                                 (album-id-list list))
  (when album-id-list
    (delete-from db "albums"
     :where `(:in "album_id" (:p ,(mapcar #'mita.id:to-string
                                          album-id-list))))))

(defmethod mita.db:album-insert ((db rdb)
                                 (albums list))
  (insert-into db "albums" '("album_id" "name" "created_on")
               (mapcar
                (lambda (album)
                  (list (mita.id:to-string (mita.db:album-id album))
                        (mita.db:album-name album)
                        (local-time:to-rfc3339-timestring
                         (mita.db:album-created-on album))))
                albums)))

(defmethod mita.db:album-select ((db rdb)
                                 (album-id-list list))
  (when album-id-list
    (mapcar (lambda (row)
              (mita.db:make-album
               :id (mita.id:parse (first row))
               :name (second row)
               :created-on (local-time:universal-to-timestamp (third row))))
            (select-from db "*" "albums"
             :where `(:in "album_id" (:p ,(mapcar #'mita.id:to-string
                                                  album-id-list)))))))

(defmethod mita.db:album-thumbnail-image-delete ((db rdb)
                                                 (album-id-list list))
  (when album-id-list
    (delete-from db "album_thumbnail_image"
     :where `(:in "album_id" (:p ,(mapcar #'mita.id:to-string
                                          album-id-list))))))

(defmethod mita.db:album-thumbnail-image-select ((db rdb)
                                                 (album-id-list list))
  (when album-id-list
    (mapcar (lambda (row)
              (mita.db:make-album-thumbnail-image
               :album-id (mita.id:parse (first row))
               :image-id (mita.id:parse (second row))))
            (select-from db "*" "album_thumbnail_image"
             :where `(:in "album_id" (:p ,(mapcar #'mita.id:to-string
                                                  album-id-list)))))))

(defmethod mita.db:album-thumbnail-image-insert ((db rdb)
                                                 (rows list))
  (insert-into db "album_thumbnail_image" '("album_id" "image_id")
               (mapcar
                (lambda (row)
                  (list (mita.id:to-string
                         (mita.db:album-thumbnail-image-album-id row))
                        (mita.id:to-string
                         (mita.db:album-thumbnail-image-image-id row))))
                rows)))


(defmethod mita.db:album-image-insert ((db rdb)
                                       (album-id mita.id:id)
                                       (images list))
  (insert-into db "album_image" '("album_id" "image_id")
               (mapcar
                (lambda (image)
                  (list (mita.id:to-string album-id)
                        (mita.id:to-string (mita.image:image-id image))))
                images)))

(defmethod mita.db:album-image-delete ((db rdb)
                                       (album-id-list list))
  (when album-id-list
    (delete-from db "album_image"
     :where `(:in "album_id"
                  (:p ,(mapcar #'mita.id:to-string album-id-list))))))

(defmethod mita.db:album-image-select ((db rdb)
                                       (album-id mita.id:id))
  (mapcar #'parse-image
          (select-from db
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


(defmethod mita.db:tag-delete ((db rdb)
                               (tag-id-list list))
  (when tag-id-list
    (delete-from db "tags"
     :where `(:in "tag_id" (:p ,(mapcar #'mita.id:to-string tag-id-list))))))
                  

(defmethod mita.db:tag-select ((db rdb))
  (mapcar #'parse-tag
          (select-from db "*" "tags" :order-by "added_on")))

(defmethod mita.db:tag-insert ((db rdb)
                               (tag mita.tag:tag))
  (insert-into db "tags" '("tag_id" "name" "added_on")
               (list
                (list (mita.id:to-string (mita.tag:tag-id tag))
                      (mita.tag:tag-name tag)
                      (local-time:to-rfc3339-timestring
                       (local-time:now))))))

(defmethod mita.db:tag-content-delete ((db rdb)
                                       (tag-id mita.id:id))
  (delete-from db "tag_content"
   :where `(:= "tag_id" (:p ,(mita.id:to-string tag-id)))))

(defmethod mita.db:tag-content-delete-by-content ((db rdb)
                                                  (content-id mita.id:id))
  (delete-from db "tag_content"
   :where `(:= "content_id" (:p ,(mita.id:to-string content-id)))))

(defmethod mita.db:tag-content-select ((db rdb)
                                       (tag-id mita.id:id))
  (mapcar (lambda (row)
            (mita.db:make-content
             :id (mita.id:parse (first row))
             :type (alexandria:make-keyword (second row))))
          (select-from db "content_id, content_type" "tag_content"
           :where `(:= "tag_id" (:p ,(mita.id:to-string tag-id)))
           :order-by "added_on")))

(defmethod mita.db:tag-content-select-tags ((db rdb)
                                            (content-id mita.id:id))
  (mapcar #'parse-tag
          (select-from db
                       "t.tag_id, t.name"
                       "tags AS t
                          INNER JOIN tag_content
                          ON
                            t.tag_id = tag_content.tag_id"
           :where `(:= "content_id" (:p ,(mita.id:to-string content-id))))))
                       

(defmethod mita.db:tag-content-insert ((db rdb)
                                       (tag-id mita.id:id)
                                       (contents list))
  (let ((added-on (local-time:to-rfc3339-timestring (local-time:now))))
    (insert-into db "tag_content"
                 '("tag_id" "content_type" "content_id" "added_on")
                 (mapcar
                  (lambda (content)
                    (list (mita.id:to-string tag-id)
                          (string (mita.db:content-type content))
                          (mita.id:to-string (mita.db:content-id content))
                          added-on))
                  contents))))

(defmethod mita.db:tag-content-insert-by-tags ((db rdb)
                                               (tag-id-list list)
                                               (content mita.db:content))
  (let ((added-on (local-time:to-rfc3339-timestring (local-time:now))))
    (insert-into db "tag_content"
                 '("tag_id" "content_type" "content_id" "added_on")
                 (mapcar
                  (lambda (tag-id)
                    (list (mita.id:to-string tag-id)
                          (string (mita.db:content-type content))
                          (mita.id:to-string (mita.db:content-id content))
                          added-on))
                  tag-id-list))))
