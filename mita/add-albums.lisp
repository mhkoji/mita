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

(defun folder-list-contents-only (folder)
  (let ((children (remove-if #'mita.fs:folder-p
                             (mita.fs:folder-list-children folder))))
    (funcall *sort-fn* children)))

(defun create-source (thumbnail-folder folder)
  (mita.album:make-album-source
   :id (mita.id:gen-from-name (mita.fs:file-path folder))
   :name (mita.fs:file-path folder)
   :created-on (mita.fs:file-created-on folder)
   :thumbnail
   (let ((file (car (folder-list-contents-only folder))))
     (let ((thumbnail-file (mita.fs:make-thumbnail thumbnail-folder file)))
       (make-image mita.image:+source-thumbnail+ thumbnail-file)))))

(defun run (conn folders thumbnail-folder)
  (setq folders (remove-if (lambda (folder)
                             (null (folder-list-contents-only folder)))
                           folders))
  (when-let ((sources (mapcar (lambda (folder)
                                (create-source thumbnail-folder folder))
                              folders)))
    (let ((albums (mita.album:create-with-images conn sources)))
      ;; Update images
      (loop for folder in folders
            for album in albums
            do (let ((images
                      (mapcar (lambda (p)
                                (make-image mita.image:+source-content+ p))
                              (folder-list-contents-only folder))))
                 (mita.image:delete-images
                  conn (mapcar #'mita.image:image-id images))
                 (mita.image:save-images conn images)
                 (mita.album:update-album-images conn album images)))))
  (values))
