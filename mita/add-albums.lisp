(defpackage :mita.add-albums
  (:use :cl :mita.dir)
  (:export :run))
(in-package :mita.add-albums)

(defun make-image (file)
  (mita.image:make-image
   :id (mita.id:gen-from-name (mita.dir:file-path file))
   :path (namestring (mita.dir:file-full-path file))))

(defun make-thumbnail-path (thumbnail-dir source-path)
  (format nil "~Athumbnail$~A"
          thumbnail-dir
          (cl-ppcre:regex-replace-all "/" source-path "$")))

(defun file-dir-list-contents-only (file)
  (remove-if #'file-dir-p (file-dir-list file)))

(defun create-source (thumbnail-dir file)
  (mita.album:make-album-source
   :id
   (mita.id:gen-from-name (mita.dir:file-path file))
   :name
   (namestring (mita.dir:file-path file))
   :created-on
   (local-time:universal-to-timestamp
    (file-write-date (mita.dir:file-full-path file)))
   :thumbnail
   (alexandria:when-let ((file (car (file-dir-list-contents-only file))))
     (let ((thumbnail-path (make-thumbnail-path
                            thumbnail-dir
                            (mita.dir:file-path file))))
       (mita:create-thumbnail thumbnail-path
                              (mita.dir:file-full-path file))
       (make-image (mita.dir:as-file thumbnail-dir thumbnail-path))))))

(defun run (gw dirs thumbnail-dir)
  (let ((sources (mapcar (lambda (d)
                           (create-source thumbnail-dir d))
                         dirs)))
    (mita.album:delete-albums gw
     (mapcar #'mita.album:album-source-id sources))
    (mita.image:delete-images gw
     (mapcar #'mita.image:image-id
             (remove nil (mapcar #'mita.album:album-source-thumbnail
                                 sources))))
    (mita.image:save-images gw
     (remove nil (mapcar #'mita.album:album-source-thumbnail sources)))
    (let ((albums (mita.album:create-albums gw sources)))
      ;; Update images
      (loop for dir in dirs
            for album in albums
            do (let ((images (mapcar #'make-image
                                     (file-dir-list-contents-only dir))))
                 (mita.image:delete-images gw (mapcar #'mita.image:image-id
                                                      images))
                 (mita.image:save-images gw images)
                 (mita.album:update-album-images gw album images)))))
  (values))
