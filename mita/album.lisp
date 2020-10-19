(defpackage :mita.album
  (:use :cl :mita)
  (:export :album
           :album-id
           :album-name
           :album-thumbnail
           :delete-albums
           :create-albums
           :load-album-by-id
           :load-albums
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

(defun load-albums-in (gateway album-id-list)
  (let ((db (gateway-db gateway)))
    (when-let ((album-rows (mita.db:album-select db album-id-list)))
      (let ((id->args (make-hash-table :test #'equal)))
        (dolist (album-row album-rows)
          (let ((album-id (mita.db:album-id album-row)))
            (setf (gethash (mita.id:to-string album-id) id->args)
                  (list :album-row album-row))))
        (let* ((album-thumbnail-image-row-list
                (mita.db:album-thumbnail-image-select db album-id-list))
               (image-list
                (mita.image:load-images-by-ids
                 gateway
                 (mapcar #'mita.db:album-thumbnail-image-image-id
                         album-thumbnail-image-row-list))))
          (loop for image in image-list
                for row in album-thumbnail-image-row-list
                do (let ((album-id
                          (mita.db:album-thumbnail-image-album-id row)))
                     (alexandria:appendf
                      (gethash (mita.id:to-string album-id) id->args)
                      (list :thumbnail image)))))
        (mapcar (lambda (id)
                  (let ((args (gethash (mita.id:to-string id) id->args)))
                    (apply #'make-instance 'album args)))
                album-id-list)))))

(defun load-albums (gateway offset limit)
  (let ((ids (mita.db:album-select-album-ids (gateway-db gateway)
                                             offset
                                             limit)))
    (load-albums-in gateway ids)))

(defun load-album-by-id (gateway album-id)
  (car (load-albums-in gateway (list album-id))))


(defun delete-albums (gateway album-id-list)
  (let ((db (gateway-db gateway)))
    (mita.db:album-thumbnail-image-delete db album-id-list)
    (mita.db:album-image-delete db album-id-list)
    (mita.db:album-delete db album-id-list)))

(defstruct album-source id name created-on thumbnail)

(defun create-albums (gateway sources)
  (let ((db (gateway-db gateway)))
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
             (remove-if-not #'album-source-thumbnail sources))))
  (load-albums-in gateway (mapcar #'album-source-id sources)))


(defun album-images (gateway album)
  (mita.db:album-image-select (gateway-db gateway) (album-id album)))

(defun update-album-images (gateway album images)
  (let ((db (gateway-db gateway)))
    (mita.db:album-image-delete db (list (album-id album)))
    (mita.db:album-image-insert db (album-id album) images))
  (values))
