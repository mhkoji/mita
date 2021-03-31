(defpackage :mita.add-albums
  (:use :cl)
  (:export :run
           :*sort-fn*)
  (:import-from :alexandria
                :when-let))
(in-package :mita.add-albums)

(defvar *sort-fn* #'identity)

(defun make-image (source file)
  (mita.image:make-image
   :source source
   :id (mita.id:gen-from-name (mita.fs:file-path file))
   :path (mita.fs:file-path file)))

(defun dir-list-contents-only (dir)
  (let ((children (remove-if #'mita.fs:dir-p
                             (mita.fs:dir-list-children dir))))
    (funcall *sort-fn* children)))

(defun create-source (thumbnail-dir file)
  (mita.album:make-album-source
   :id (mita.id:gen-from-name (mita.fs:file-path file))
   :name (mita.fs:file-path file)
   :created-on (mita.fs:file-created-on file)
   :thumbnail
   (let ((file (car (dir-list-contents-only file))))
     (let ((thumbnail-file (mita.fs:make-thumbnail thumbnail-dir file)))
       (make-image mita.image:+source-thumbnail+ thumbnail-file)))))

(defun run (db dirs thumbnail-dir)
  (setq dirs (remove-if (lambda (dir)
                          (null (dir-list-contents-only dir)))
                        dirs))
  (when-let ((sources (mapcar (lambda (d)
                                (create-source thumbnail-dir d))
                              dirs)))
    (let ((albums (mita.album:create-with-images db sources)))
      ;; Update images
      (loop for dir in dirs
            for album in albums
            do (let ((images
                      (mapcar (lambda (p)
                                (make-image mita.image:+source-content+ p))
                              (dir-list-contents-only dir))))
                 (mita.image:delete-images
                  db (mapcar #'mita.image:image-id images))
                 (mita.image:save-images db images)
                 (mita.album:update-album-images db album images)))))
  (values))
