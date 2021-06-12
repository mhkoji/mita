(in-package :mita.db.bpt)

(defmethod mita.album:save-albums ((conn connection) (sources list))
  (let ((tree (album-tree conn)))
    (dolist (s sources)
      (let ((key (mita.id:to-string (mita.album:album-source-id s)))
            (value (list :id
                         (mita.album:album-source-id s)
                         :name
                         (mita.album:album-source-name s)
                         :created-on
                         (mita.album:album-source-created-on s)
                         :thumbnail
                         (mita.image:image-id
                          (mita.album:album-source-thumbnail s)))))
        (mita.util.b+tree:insert tree key value)))))

(defmethod mita.album:load-albums-in ((conn connection)
                                      (album-ids list))
  (when album-ids
    (mapcar (lambda (value)
              (mita.album:construct-album
               :id (getf value :id)
               :name (getf value :name)
               :thumbnail
               #+nil ;; for debugging
               (getf value :thumbnail)
               (mita.image:load-image conn (getf value :thumbnail))))
            (mita.util.b+tree:search-in
             (album-tree conn)
             (mapcar #'mita.id:to-string album-ids)))))
