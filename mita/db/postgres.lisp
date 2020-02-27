(defpackage :mita.db.postgres
  (:use :cl)
  (:import-from :alexandria
                :when-let)
  (:export :postgres
           :make-connector
           :with-transaction))
(in-package :mita.db.postgres)

(defclass postgres (mita.db:db)
  ((conn
    :initarg :conn
    :reader postgres-conn)))

(defstruct connector
  database user password host port)

(defun connect (connector)
  (with-slots (database user password host port) connector
    (let ((conn (postmodern:connect database user password host
                                    :port port)))
      (make-instance 'postgres :conn conn))))

(defun disconnect (db)
  (postmodern:disconnect (postgres-conn db)))

(defmacro with-transaction ((db connector) &body body)
  `(let ((,db (connect ,connector)))
     (unwind-protect
          (let ((postmodern:*database* (postgres-conn ,db)))
            (postmodern:with-transaction (nil :serializable)
              ,@body))
       (disconnect ,db))))

(defun query (db query-string args)
  (let ((conn (postgres-conn db)))
    (cl-postgres:prepare-query conn "" query-string)
    (cl-postgres:exec-prepared conn "" args #'cl-postgres:list-row-reader)))

(defun single (row-parser db query-string args)
  (car (mapcar row-parser (query db query-string args))))

(defclass page (mita.page:page)
  ((id
    :initarg :id
    :type 'mita.id:id
    :reader mita.page:page-id)
   (created-on
    :initarg :created-on
    :type 'local-time:timestamp
    :reader mita.page:page-created-on)))

(defun parse-page (row)
  (make-instance 'page
   :id (mita.id:parse (first row))
   :created-on (local-time:universal-to-timestamp (second row))))

(defmethod mita.db:page-delete ((db postgres)
                                (page-id-list list))
  (when page-id-list
    (query db
           (with-output-to-string (s)
             (format s "DELETE FROM pages WHERE page_id in (")
             (format s "~{~A~^,~}"
                       (loop for i from 1 to (length page-id-list)
                             collect (format nil "$~A" i)))
             (format s ")"))
           (mapcar #'mita.id:to-string page-id-list))))

(defmethod mita.db:page-insert ((db postgres)
                                (page-id mita.id:id))
  (query db
         "INSERT INTO pages (page_id, created_on) VALUES ($1, $2)"
         (list
          (mita.id:to-string page-id)
          (local-time:to-rfc3339-timestring (local-time:now)))))

(defmethod mita.db:page-select-by-id ((db postgres)
                                      (page-id mita.id:id))
  (single #'parse-page db
          "SELECT * FROM pages WHERE page_id = $1"
          (list
           (mita.id:to-string page-id))))

(defmethod mita.db:page-select ((db postgres))
  (mapcar #'parse-page (query db "SELECT * FROM pages" nil)))


(defmethod mita.db:page-text-delete ((db postgres)
                                     (page-id-list list))
  (when page-id-list
    (query db
           (with-output-to-string (s)
             (format s "DELETE FROM page_text WHERE page_id in (")
             (format s "~{~A~^,~}"
                       (loop for i from 1 to (length page-id-list)
                             collect (format nil "$~A" i)))
             (format s ")"))
           (mapcar #'mita.id:to-string page-id-list))))

(defmethod mita.db:page-text-insert ((db postgres)
                                     (page-id mita.id:id)
                                     (text string))
  (query db
         "INSERT INTO page_text (page_id, string) VALUES ($1, $2)"
         (list
          (mita.id:to-string page-id)
          text)))

(defmethod mita.db:page-text-select ((db postgres)
                                     (page-id mita.id:id))
  (single #'first db
          "SELECT string FROM page_text WHERE page_id = $1"
          (list
           (mita.id:to-string page-id))))

(defmethod mita.db:page-text-update ((db postgres)
                                     (page-id mita.id:id)
                                     (text string))
  (query db
         "UPDATE page_text set string = $1 where page_id = $2"
         (list
          text
          (mita.id:to-string page-id))))


(defun parse-image (row)
  (mita.image:make-image
   :id (mita.id:parse (first row))
   :path (second row)))

(defmethod mita.db:image-select-by-ids ((db postgres)
                                        (image-id-list list))
  (mapcar #'parse-image
          (query db
                 (with-output-to-string (s)
                   (format s "SELECT * FROM images WHERE image_id in (")
                   (format s "~{~A~^,~}"
                           (loop for i from 1 to (length image-id-list)
                                 collect (format nil "$~A" i)))
                   (format s ")"))
                 (mapcar #'mita.id:to-string image-id-list))))

(defmethod mita.db:image-insert ((db postgres)
                                 (images list))
  (query db
         (with-output-to-string (s)
           (format s "INSERT INTO images (image_id, path) VALUES ")
           (format s "~{~A~^,~}"
                   (let ((i 0))
                     (loop repeat (length images)
                           for for-image-id = (incf i)
                           for for-path = (incf i)
                           collect (list (format nil "$~A, $~A"
                                                 for-image-id
                                                 for-path))))))
         (alexandria:mappend
          (lambda (image)
            (list (mita.id:to-string (mita.image:image-id image))
                  (mita.image:image-path image)))
          images)))


(defmethod mita.db:page-image-insert ((db postgres)
                                      (page-id mita.id:id)
                                      (images list))
  (query db
         (with-output-to-string (s)
           (format s "INSERT INTO page_image (page_id, image_id) VALUES")
           (format s "~{~A~^,~}"
                   (let ((i 0))
                     (loop repeat (length images)
                           for for-page-id = (incf i)
                           for for-image-id = (incf i)
                           collect (list (format nil "$~A, $~A"
                                                 for-page-id
                                                 for-image-id))))))
         (alexandria:mappend
          (lambda (image)
            (list (mita.id:to-string page-id)
                  (mita.id:to-string (mita.image:image-id image))))
          images)))

(defmethod mita.db:page-image-delete ((db postgres)
                                      (page-id mita.id:id))
  (query db
         "DELETE FROM page_image WHERE page_id = $1"
         (list (mita.id:to-string page-id))))

(defmethod mita.db:page-image-select ((db postgres)
                                      (page-id mita.id:id))
  (mapcar #'parse-image
          (query db
                 "SELECT i.image_id, i.path FROM images AS i
                    INNER JOIN page_image ON
                       i.image_id = page_image.image_id
                    WHERE
                       page_image.page_id = $1"
                 (list (mita.id:to-string page-id)))))


(defmethod mita.db:album-delete ((db postgres)
                                 (album-id-list list))
  (when album-id-list
    (query db
           (with-output-to-string (s)
             (format s "DELETE FROM albums WHERE album_id in (")
             (format s "~{~A~^,~}"
                     (loop for i from 1 to (length album-id-list)
                           collect (format nil "$~A" i)))
             (format s ")"))
           (mapcar #'mita.id:to-string album-id-list))))

(defmethod mita.db:album-insert ((db postgres)
                                 (albums list))
  (query db
         (with-output-to-string (s)
           (format s "INSERT INTO albums (album_id, name, created_on) VALUES")
           (format s "~{~A~^,~}"
                   (let ((i 0))
                     (loop repeat (length albums)
                           for i1 = (incf i)
                           for i2 = (incf i)
                           for i3 = (incf i)
                           collect (list (format nil "$~A, $~A, $~A"
                                                 i1 i2 i3))))))

         (alexandria:mappend
          (lambda (album)
            (list (mita.id:to-string (mita.db:album-id album))
                  (mita.db:album-name album)
                  (local-time:to-rfc3339-timestring
                   (mita.db:album-created-on album))))
          albums)))

(defmethod mita.db:album-select ((db postgres)
                                 (album-id-list list))
  (when album-id-list
    (mapcar (lambda (row)
              (mita.db:make-album
               :id (mita.id:parse (first row))
               :name (second row)
               :created-on (local-time:universal-to-timestamp (third row))))
            (query db
                   (with-output-to-string (s)
                     (format s "SELECT * FROM albums WHERE album_id in (")
                     (format s "~{~A~^,~}"
                             (loop for i from 1 to (length album-id-list)
                                   collect (format nil "$~A" i)))
                     (format s ")"))
                   (mapcar #'mita.id:to-string album-id-list)))))

(defmethod mita.db:album-select-album-ids ((db postgres) offset limit)
  (mapcar (lambda (row)
            (mita.id:parse (car row)))
          (query db
                 "SELECT album_id FROM albums OFFSET $1 LIMIT $2"
                 (list offset limit))))


(defmethod mita.db:album-thumbnail-image-delete ((db postgres)
                                                 (album-id-list list))
  (when album-id-list
    (query db
           (with-output-to-string (s)
             (format s "DELETE FROM album_thumbnail_image")
             (format s " WHERE album_id in (")
             (format s "~{~A~^,~}"
                     (loop for i from 1 to (length album-id-list)
                           collect (format nil "$~A" i)))
             (format s ")"))
           (mapcar #'mita.id:to-string album-id-list))))

(defmethod mita.db:album-thumbnail-image-select ((db postgres)
                                                 (album-id-list list))
  (when album-id-list
    (mapcar (lambda (row)
              (mita.db:make-album-thumbnail-image
               :album-id (mita.id:parse (first row))
               :image-id (mita.id:parse (second row))))
            (query db
                   (with-output-to-string (s)
                     (format s "SELECT * FROM album_thumbnail_image")
                     (format s " WHERE album_id in (")
                     (format s "~{~A~^,~}"
                             (loop for i from 1 to (length album-id-list)
                                   collect (format nil "$~A" i)))
                     (format s ")"))
                 (mapcar #'mita.id:to-string album-id-list)))))

(defmethod mita.db:album-thumbnail-image-insert ((db postgres)
                                                 (rows list))
  (query db
         (with-output-to-string (s)
           (format s "INSERT INTO album_thumbnail_image")
           (format s " (album_id, image_id) VALUES")
           (format s "~{~A~^,~}"
                   (let ((i 0))
                     (loop repeat (length rows)
                           for i1 = (incf i)
                           for i2 = (incf i)
                           collect (list (format nil "$~A, $~A" i1 i2))))))
         (alexandria:mappend
          (lambda (row)
            (list (mita.id:to-string
                   (mita.db:album-thumbnail-image-album-id row))
                  (mita.id:to-string
                   (mita.db:album-thumbnail-image-image-id row))))
          rows)))


(defmethod mita.db:album-image-insert ((db postgres)
                                      (album-id mita.id:id)
                                      (images list))
  (query db
         (with-output-to-string (s)
           (format s "INSERT INTO album_image (album_id, image_id) VALUES")
           (format s "~{~A~^,~}"
                   (let ((i 0))
                     (loop repeat (length images)
                           for for-album-id = (incf i)
                           for for-image-id = (incf i)
                           collect (list (format nil "$~A, $~A"
                                                 for-album-id
                                                 for-image-id))))))
         (alexandria:mappend
          (lambda (image)
            (list (mita.id:to-string album-id)
                  (mita.id:to-string (mita.image:image-id image))))
          images)))

(defmethod mita.db:album-image-delete ((db postgres)
                                       (album-id-list list))
  (query db
         (with-output-to-string (s)
           (format s "DELETE FROM album_image WHERE album_id in (")
           (format s "~{~A~^,~}"
                   (loop for i from 1 to (length album-id-list)
                         collect (format nil "$~A" i)))
           (format s ")"))
         (mapcar #'mita.id:to-string album-id-list)))

(defmethod mita.db:album-image-select ((db postgres)
                                       (album-id mita.id:id))
  (mapcar #'parse-image
          (query db
                 "SELECT i.image_id, i.path FROM images AS i
                    INNER JOIN album_image ON
                       i.image_id = album_image.image_id
                    WHERE
                       album_image.album_id = $1"
                 (list (mita.id:to-string album-id)))))
