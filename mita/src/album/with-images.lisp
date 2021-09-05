(in-package :mita.album)

(defun delete-with-images (conn album-ids)
  (let ((existing-albums (load-albums-in conn album-ids)))
    (let ((album-images (alexandria:mappend (lambda (a)
                                              (album-images conn a))
                                            existing-albums))
          (album-thumbnails (remove nil (mapcar #'album-thumbnail
                                                existing-albums))))
      (delete-albums
       conn (mapcar #'album-id existing-albums))
      (mita.image:delete-images
       conn (mapcar #'mita.image:image-id album-thumbnails))
      (mita.image:delete-images
       conn (mapcar #'mita.image:image-id album-images))))
  (values))

(defun create-with-images (conn sources)
  (delete-with-images
   conn (mapcar #'album-source-id sources))
  (mita.image:save-images
   conn (remove nil (mapcar #'album-source-thumbnail sources)))
  (create-albums conn sources))
