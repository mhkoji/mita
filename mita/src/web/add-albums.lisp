(defpackage :mita.web.add-albums
  (:use :cl)
  (:export :run
           :*sort-fn*)
  (:import-from :alexandria
                :when-let))
(in-package :mita.web.add-albums)

(defvar *sort-fn* #'identity)

(defun make-image (source file)
  (mita.image:make-image
   :source source
   :id (mita.id:gen-from-name (mita.file:file-path file))
   :path (mita.file:file-path file)))

(defun list-files-sorted (content-repos folder)
  (funcall *sort-fn* (remove-if
                      #'mita.file:folder-p
                      (mita.file:folder-list-children content-repos folder))))

(defun create-source (thumbnail-repos content-repos folder)
  (mita.album:make-album-source
   :id (mita.id:gen-from-name (mita.file:file-path folder))
   :name (mita.file:file-path folder)
   :created-on (mita.file:file-created-on folder)
   :thumbnail (let ((file (car (list-files-sorted content-repos folder))))
                (let ((thumbnail-file (mita.file:make-thumbnail
                                       thumbnail-repos
                                       file)))
                  (make-image mita.image:+source-thumbnail+ thumbnail-file)))))

(defun run (conn thumbnail-repos content-repos folders)
  (setq folders
        (remove-if (lambda (folder)
                     (null (list-files-sorted content-repos folder)))
                   folders))
  (when-let ((sources (mapcar (lambda (folder)
                                (create-source thumbnail-repos content-repos folder))
                              folders)))
    (let ((albums (mita.album:create-with-images conn sources)))
      ;; Update images
      (loop for folder in folders
            for album in albums
            do (let ((images (mapcar (lambda (p)
                                       (make-image mita.image:+source-content+ p))
                                     (list-files-sorted content-repos folder))))
                 (mita.image:delete-images
                  conn (mapcar #'mita.image:image-id images))
                 (mita.image:save-images conn images)
                 (mita.album:update-album-images conn album images)))))
  (values))
