(in-package :mita.album)

(defun delete-with-images (db sources)
  (let ((existing-albums
         (mita.album:load-albums-in
          db (mapcar #'album-source-id sources))))
    (let ((album-images
           (alexandria:mappend (lambda (a) (album-images db a))
                               existing-albums))
          (album-thumbnails
           (remove nil (mapcar #'album-thumbnail existing-albums))))
      (delete-albums
       db (mapcar #'album-id existing-albums))
      (mita.image:delete-images
       db (mapcar #'mita.image:image-id album-thumbnails))
      (mita.image:delete-images
       db (mapcar #'mita.image:image-id album-images))))
  (values))

(defun create-with-images (db sources)
  (delete-with-images db sources)
  (mita.image:save-images
   db (remove nil (mapcar #'album-source-thumbnail sources)))
  (create-albums db sources))
