(defpackage :mita.add-albums
  (:use :cl :mita.dir)
  (:export :run))
(in-package :mita.add-albums)

(defun make-image (path)
  (mita.image:make-image
   :id (mita.id:gen-from-name path)
   :path path))

(defun create-source (thumbnail-dir dir)
  (mita.album:make-album-source
   :id
   (mita.id:gen-from-name (dir-path dir))
   :name
   (dir-path dir)
   :created-on
   (local-time:universal-to-timestamp (dir-write-date dir))
   :thumbnail
   (alexandria:when-let ((paths (dir-file-paths dir)))
     (make-image (mita:create-thumbnail thumbnail-dir (car paths))))))

(defun run (connector thumbnail-dir root-dir &key (sort-paths-fn #'identity))
  (let ((dirs (mita.dir:retrieve root-dir sort-paths-fn)))
    (mita:with-gateway (gw connector)
      (let ((sources (mapcar (lambda (d)
                               (create-source thumbnail-dir d))
                             dirs)))
        ;; Delete existing albums if any
        (mita.album:delete-albums gw
         (mapcar #'mita.album:album-source-id sources))
        ;; Save albums
        (mita.image:save-images gw
         (mapcar #'mita.album:album-source-thumbnail sources))
        (let ((albums (mita.album:create-albums gw sources)))
          ;; Update images
          (loop for dir in dirs
                for album in albums
                do (let ((images (mapcar #'make-image (dir-file-paths dir))))
                     (mita.image:save-images gw images)
                     (mita.album:update-album-images gw album images)))))))
  (values))
