(defpackage :mita.db.file
  (:use :cl)
  (:export :make-connector
           :account-db-name
           :with-admin-db
           :with-db
           :file-db))
(in-package :mita.db.file)

(defgeneric account-db-name (account))

(defstruct connector dir)

(defclass file-db (mita.db:db)
  ((table-hash
    :initarg :table-hash
    :reader table-hash)))

(defvar *file-mutex*
  (sb-thread:make-mutex :name "file-mutex"))

(defun read-table (path)
  (labels ((do-read ()
             (with-open-file (in path)
               (cl-csv:read-csv in))))
    (sb-thread:with-mutex (*file-mutex*)
      (do-read))))

(defun write-table-hash (dir table-hash)
  (labels ((do-write ()
             (ensure-directories-exist dir)
             (maphash (lambda (name rows)
                        (let ((path (format nil "~A/~A.csv" dir name)))
                          (when (uiop:file-exists-p path)
                            (delete-file path))
                          (with-open-file (out path :direction :output)
                            (cl-csv:write-csv rows :stream out)
                            (force-output out))))
                      table-hash)))
    (sb-thread:with-mutex (*file-mutex*)
      (do-write))))

(defun read-into-table-hash (dir)
  (let ((hash (make-hash-table :test #'equal)))
    (dolist (path (uiop:directory-files dir))
      (let ((table-name (pathname-name path)))
        (setf (gethash table-name hash) (read-table path))))
    hash))

(defmacro with-file-db ((db dir) &body body)
  (let ((g (gensym)))
    `(let ((,g ,dir))
       (let ((,db (make-instance
                   'file-db
                   :table-hash (read-into-table-hash ,g))))
         (unwind-protect
              (progn ,@body)
           (write-table-hash ,g (table-hash ,db)))))))

(defmacro with-admin-db ((db connector) &body body)
  `(with-file-db (,db (connector-dir ,connector))
     ,@body))

(defmacro with-db ((db account connector) &body body)
  `(with-file-db (,db (format nil "~A/~A/"
                              (connector-dir ,connector)
                              (account-db-name ,account)))
     ,@body))

(defvar +image+ "image")
(defvar +album+ "album")
(defvar +album-thumbnail-image+ "album-thumbnail-image")
(defvar +album-image+ "album-image")

(defun select-rows-if (db name row-pred)
  (remove-if-not row-pred (gethash name (table-hash db))))

(defun delete-rows-if (db name row-pred)
  (let ((rows (remove-if row-pred (gethash name (table-hash db)))))
    (setf (gethash name (table-hash db)) rows)))

(defun contains-id-p (id id-list)
  (member id id-list :test #'mita.id:id=))

(defmethod mita.db:image-select-by-ids ((db file-db)
                                        (image-id-list list))
  (mapcar (lambda (row)
            (mita.image:make-image
             :id (mita.id:parse (first row))
             :source (alexandria:make-keyword (second row))
             :path (third row)))
          (select-rows-if db +image+
                          (lambda (row)
                            (contains-id-p
                             (mita.id:parse (first row))
                             image-id-list)))))

(defmethod mita.db:image-insert ((db file-db)
                                 (images list))
  (let ((inserted-rows
         (mapcar
          (lambda (image)
            (list (mita.id:to-string (mita.image:image-id image))
                  (symbol-name (mita.image:image-source image))
                  (mita.image:image-path image)))
          images)))
    (alexandria:appendf (gethash +image+ (table-hash db)) inserted-rows)))

(defmethod mita.db:image-delete ((db file-db)
                                 (image-id-list list))
  (delete-rows-if db +image+
                  (lambda (row)
                    (contains-id-p (mita.id:parse (first row))
                                   image-id-list))))

(defmethod mita.db:album-delete ((db file-db)
                                 (album-id-list list))
  (delete-rows-if db +album+
                  (lambda (row)
                    (contains-id-p (mita.id:parse (first row))
                                   album-id-list))))

(defmethod mita.db:album-insert ((db file-db)
                                 (albums list))
  (let ((inserted-rows
         (mapcar
          (lambda (album)
            (list (mita.id:to-string (mita.db:album-id album))
                  (mita.db:album-name album)
                  (local-time:to-rfc3339-timestring
                   (mita.db:album-created-on album))))
          albums)))
    (alexandria:appendf (gethash +album+ (table-hash db)) inserted-rows)))

(defmethod mita.db:album-select ((db file-db)
                                 (album-id-list list))
  (mapcar (lambda (row)
            (mita.db:make-album
             :id (mita.id:parse (first row))
             :name (second row)
             :created-on (local-time:parse-rfc3339-timestring (third row))))
          (select-rows-if db +album+
                          (lambda (row)
                            (contains-id-p (mita.id:parse (first row))
                                           album-id-list)))))

(defmethod mita.db:album-select-album-ids ((db file-db) offset limit)
  (let* ((rows (copy-list (gethash +album+ (table-hash db))))
         (length (length rows)))
    (mapcar (lambda (row)
              (mita.id:parse (car row)))
            (subseq (sort rows #'>
                          :key (lambda (row)
                                 (local-time:timestamp-to-unix
                                  (local-time:parse-rfc3339-timestring
                                   (third row)))))
                    (min offset length)
                    (min (+ offset limit) length)))))


(defmethod mita.db:album-thumbnail-image-delete ((db file-db)
                                                 (album-id-list list))
  (when album-id-list
    (delete-rows-if db +album-thumbnail-image+
                    (lambda (row)
                      (contains-id-p (mita.id:parse (first row))
                                     album-id-list)))))

(defmethod mita.db:album-thumbnail-image-select ((db file-db)
                                                 (album-id-list list))
  (when album-id-list
    (mapcar (lambda (row)
              (mita.db:make-album-thumbnail-image
               :album-id (mita.id:parse (first row))
               :image-id (mita.id:parse (second row))))
            (select-rows-if db +album-thumbnail-image+
                            (lambda (row)
                              (contains-id-p (mita.id:parse (first row))
                                             album-id-list))))))

(defmethod mita.db:album-thumbnail-image-insert ((db file-db)
                                                 (rows list))
  (let ((inserted-rows
         (mapcar
          (lambda (row)
                  (list (mita.id:to-string
                         (mita.db:album-thumbnail-image-album-id row))
                        (mita.id:to-string
                         (mita.db:album-thumbnail-image-image-id row))))
          rows)))
    (alexandria:appendf (gethash +album-thumbnail-image+ (table-hash db))
                        inserted-rows)))

(defmethod mita.db:album-image-insert ((db file-db)
                                       (album-id mita.id:id)
                                       (images list))
  (let ((inserted-rows
         (mapcar
          (lambda (image)
            (list (mita.id:to-string album-id)
                  (mita.id:to-string (mita.image:image-id image))))
          images)))
    (alexandria:appendf (gethash +album-image+ (table-hash db))
                        inserted-rows)))

(defmethod mita.db:album-image-delete ((db file-db)
                                       (album-id-list list))
  (when album-id-list
    (delete-rows-if db +album-image+
                    (lambda (row)
                      (contains-id-p (mita.id:parse (first row))
                                     album-id-list)))))

(defmethod mita.db:album-image-select ((db file-db)
                                       (album-id mita.id:id))
  (mita.db:image-select-by-ids
   db
   (mapcar (lambda (row)
             (mita.id:parse (second row)))
           (select-rows-if db +album-image+
                           (lambda (row)
                             (mita.id:id= (mita.id:parse (first row))
                                          album-id))))))
