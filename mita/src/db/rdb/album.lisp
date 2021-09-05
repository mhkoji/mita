(in-package :mita.db.rdb)

(defmethod mita.album:load-albums-in ((conn connection)
                                      (album-id-list list))
  (when-let ((album-rows (album-select conn album-id-list)))
    (let ((id->args (make-hash-table :test #'equal)))
      (dolist (album-row album-rows)
        (let ((album-id (album-id album-row)))
          (setf (gethash (mita.id:to-string album-id) id->args)
                (list :id album-id
                      :name (album-name album-row)))))
      (let ((id->image
             (make-hash-table :test #'equal))
            (album-thumbnail-image-row-list
             (album-thumbnail-image-select conn album-id-list)))
        (dolist (image (mita.image:load-images-by-ids
                        conn
                        (mapcar #'album-thumbnail-image-image-id
                                album-thumbnail-image-row-list)))
          (setf (gethash (mita.id:to-string
                          (mita.image:image-id image))
                         id->image)
                image))
        (dolist (row album-thumbnail-image-row-list)
          (let* ((album-id
                  (album-thumbnail-image-album-id row))
                 (image-id
                  (album-thumbnail-image-image-id row))
                 (image
                  (gethash (mita.id:to-string image-id) id->image)))
            (when (gethash (mita.id:to-string album-id) id->args)
              (alexandria:appendf (gethash (mita.id:to-string album-id)
                                           id->args)
                                  (list :thumbnail image))))))
      (remove nil
              (mapcar
               (lambda (id)
                 (when-let ((args (gethash (mita.id:to-string id)
                                           id->args)))
                   (apply #'mita.album:construct-album args)))
               album-id-list)))))

(defmethod mita.album:load-albums ((conn connection)
                                   (offset integer)
                                   (limit integer))
  (let ((ids (album-select-album-ids
              conn offset (1+ limit))))
    (let ((full-loaded-p (= (length ids) (1+ limit))))
      (let ((albums (mita.album:load-albums-in
                     conn
                     (if full-loaded-p (butlast ids) ids))))
        (values albums full-loaded-p)))))


(defmethod mita.album:delete-albums ((conn connection)
                                     (album-id-list list))
  (album-thumbnail-image-delete conn album-id-list)
  (album-image-delete conn album-id-list)
  (album-delete conn album-id-list))

(defmethod mita.album:save-albums ((conn connection) (sources list))
  (album-insert conn
   (mapcar (lambda (s)
             (make-album
              :id (mita.album:album-source-id s)
              :name (mita.album:album-source-name s)
              :created-on (mita.album:album-source-created-on s)))
           sources))
  (album-thumbnail-image-insert conn
   (mapcar (lambda (s)
             (make-album-thumbnail-image
              :album-id (mita.album:album-source-id s)
              :image-id (mita.image:image-id
                         (mita.album:album-source-thumbnail s))))
           (remove-if-not #'mita.album:album-source-thumbnail sources)))
  (values))


(defmethod mita.album:album-images ((conn connection)
                                    (album mita.album:album))
  (album-image-select conn (mita.album:album-id album)))

(defmethod mita.album:update-album-images ((conn connection)
                                           (album mita.album:album)
                                           (images list))
  (album-image-delete conn (list (mita.album:album-id album)))
  (album-image-insert conn (mita.album:album-id album) images)
  (values))


(defmethod mita.album.list:run ((conn connection)
                                (q mita.album.list:query)
                                (offset integer)
                                (limit integer))
  (let ((ids (album-select-album-ids-by-query conn q offset (1+ limit))))
    (let ((has-more (= (length ids) (1+ limit))))
      (let ((albums (mita.album:load-albums-in
                     conn
                     (if has-more (butlast ids) ids))))
        (values albums has-more)))))
