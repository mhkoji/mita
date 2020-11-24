(defpackage :mita.add-albums
  (:use :cl :mita.dir)
  (:export :run
           :*sort-fn*)
  (:import-from :alexandria
                :when-let))
(in-package :mita.add-albums)

(defvar *sort-fn* #'identity)

(defun make-image (source file)
  (mita.image:make-image
   :source source
   :id (mita.id:gen-from-name (mita.dir:file-path file))
   :path (namestring (mita.dir:file-path file))))

(defun make-thumbnail-path (thumbnail-dir file)
  (format nil "~Athumbnail~A"
          (cl-fad:pathname-as-directory thumbnail-dir)
          (cl-ppcre:regex-replace-all
           "/" (mita.dir:file-path file) "$")))

(defun file-dir-list-contents-only (file)
  (funcall *sort-fn* (remove-if #'file-dir-p (file-dir-list file))))

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
     (let ((thumbnail-path (make-thumbnail-path thumbnail-dir file)))
       (mita.thumbnail:create thumbnail-path
                              (mita.dir:file-full-path file))
       (make-image mita.image:+source-thumbnail+
                   (mita.dir:as-file thumbnail-dir thumbnail-path))))))

(defun run (db dirs thumbnail-dir)
  (setq dirs (remove-if (lambda (dir)
                          (null (file-dir-list-contents-only dir)))
                        dirs))
  (when-let ((sources (mapcar (lambda (d)
                                (create-source thumbnail-dir d))
                              dirs)))
    (let* ((existing-albums
            (mita.album:load-albums-in
             db
             (mapcar #'mita.album:album-source-id sources)))
           (album-images
            (alexandria:mappend (lambda (a)
                                  (mita.album:album-images db a))
                                existing-albums))
           (album-thumbnails
            (remove nil (mapcar #'mita.album:album-thumbnail
                                existing-albums))))
      (mita.album:delete-albums
       db
       (mapcar #'mita.album:album-id existing-albums))
      (mita.image:delete-images
       db
       (mapcar #'mita.image:image-id album-thumbnails))
      (mita.image:delete-images
       db
       (mapcar #'mita.image:image-id album-images)))

    (mita.image:save-images
     db
     (remove nil (mapcar #'mita.album:album-source-thumbnail
                         sources)))
    (let ((albums (mita.album:create-albums db sources)))
      ;; Update images
      (loop for dir in dirs
            for album in albums
            do (let ((images
                      (mapcar (lambda (p)
                                (make-image mita.image:+source-content+ p))
                              (file-dir-list-contents-only dir))))
                 (mita.image:delete-images db (mapcar #'mita.image:image-id
                                                      images))
                 (mita.image:save-images db images)
                 (mita.album:update-album-images db album images)))))
  (values))
