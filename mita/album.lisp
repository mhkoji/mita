(defpackage :mita.album
  (:use :cl)
  (:export :album
           :album-id
           :album-name
           :album-thumbnail
           :delete-albums
           :create-albums
           :load-album-by-id
           :load-albums
           :load-albums-in
           :album-images
           :update-album-images
           :make-album-source
           :album-source-id
           :album-source-name
           :album-source-thumbnail
           :album-source-created-on)
  (:import-from :alexandria :when-let))
(in-package :mita.album)

(defgeneric album-id (album))

(defgeneric album-name (album))

(defgeneric album-thumbnail (album))

(defclass album ()
  ((album-row
    :initarg :album-row
    :reader album-album-row)
   (thumbnail
    :initarg :thumbnail
    :reader album-thumbnail)))

(defmethod album-id ((album album))
  (mita.db:album-id (album-album-row album)))

(defmethod album-name ((album album))
  (mita.db:album-name (album-album-row album)))

(defun load-albums-in (db album-id-list)
  (when-let ((album-rows (mita.db:album-select db album-id-list)))
    (let ((id->args (make-hash-table :test #'equal)))
      (dolist (album-row album-rows)
        (let ((album-id (mita.db:album-id album-row)))
          (setf (gethash (mita.id:to-string album-id) id->args)
                (list :album-row album-row))))
      (let ((id->image
             (make-hash-table :test #'equal))
            (album-thumbnail-image-row-list
             (mita.db:album-thumbnail-image-select db album-id-list)))
        (dolist (image (mita.image:load-images-by-ids
                        db
                        (mapcar #'mita.db:album-thumbnail-image-image-id
                                album-thumbnail-image-row-list)))
          (setf (gethash (mita.id:to-string
                          (mita.image:image-id image))
                         id->image)
                image))
        (dolist (row album-thumbnail-image-row-list)
          (let* ((album-id
                  (mita.db:album-thumbnail-image-album-id row))
                 (image-id
                  (mita.db:album-thumbnail-image-image-id row))
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
                   (apply #'make-instance 'album args)))
               album-id-list)))))

(defun load-albums (db offset limit)
  (let ((ids (mita.db:album-select-album-ids db offset limit)))
    (load-albums-in db ids)))

(defun load-album-by-id (db album-id)
  (car (load-albums-in db (list album-id))))


(defun delete-albums (db album-id-list)
  (mita.db:album-thumbnail-image-delete db album-id-list)
  (mita.db:album-image-delete db album-id-list)
  (mita.db:album-delete db album-id-list))

(defstruct album-source id name created-on thumbnail)

(defun create-albums (db sources)
  (mita.db:album-insert db
   (mapcar (lambda (s)
             (mita.db:make-album
              :id (album-source-id s)
              :name (album-source-name s)
              :created-on (album-source-created-on s)))
           sources))
  (mita.db:album-thumbnail-image-insert db
   (mapcar (lambda (s)
             (mita.db:make-album-thumbnail-image
              :album-id (album-source-id s)
              :image-id (mita.image:image-id
                         (album-source-thumbnail s))))
           (remove-if-not #'album-source-thumbnail sources)))
  (load-albums-in db (mapcar #'album-source-id sources)))


(defun album-images (db album)
  (mita.db:album-image-select db (album-id album)))

(defun update-album-images (db album images)
  (mita.db:album-image-delete db (list (album-id album)))
  (mita.db:album-image-insert db (album-id album) images)
  (values))
