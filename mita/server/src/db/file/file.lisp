(defpackage :mita.db.file
  (:use :cl)
  (:export :file-db
           :create-admin-database
           :create-database
           :drop-database))
(in-package :mita.db.file)

(defvar *file-mutex*
  (sb-thread:make-mutex :name "file-mutex"))

(defun read-table (path)
  (labels ((do-read ()
             (with-open-file (in path)
               (read in))))
    (sb-thread:with-mutex (*file-mutex*)
      (do-read))))

(defun write-table-hash (dir table-hash)
  (labels ((do-write ()
             (ensure-directories-exist dir)
             (maphash (lambda (name rows)
                        (let ((path (format nil "~A/~A.lisp" dir name)))
                          (when (uiop:file-exists-p path)
                            (delete-file path))
                          (with-open-file (out path :direction :output)
                            (print rows out)
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

;;;

(defclass file-db (mita.db:db)
  ((db-name
    :initarg :db-name
    :reader file-db-db-name)
   (base-dir
    :initarg :base-dir
    :reader file-db-base-dir)))

(defclass connection (mita.db:connection)
  ((dir
    :initarg :dir
    :accessor connection-dir)
   (table-hash
    :initarg :table-hash
    :accessor connection-table-hash)))

(defmethod mita.db:call-with-connection ((db file-db) fn)
  (let ((dir (format nil "~A/~A/"
                     (file-db-base-dir db)
                     (file-db-db-name db))))
    (funcall fn (make-instance 'connection
                 :dir dir
                 :table-hash (read-into-table-hash dir)))))

(defmethod mita.db:call-with-tx ((conn connection) fn)
  (unwind-protect (funcall fn)
    (write-table-hash (connection-dir conn)
                      (connection-table-hash conn))))


(defvar +image+ "image")
(defvar +album+ "album")
(defvar +album-images+ "album-images")
(defvar +account+ "account")

(defun create-admin-database (db-dir admin-db-name dir)
  (declare (ignore db-dir))
  (mita.db:with-connection (conn (make-instance 'file-db
                                  :db-name admin-db-name
                                  :base-dir dir))
    (let ((hash (make-hash-table :test #'equal)))
      (dolist (name (list +account+))
        (setf (gethash name hash) nil))
      (setf (connection-table-hash conn) hash))))

(defun create-database (db-dir admin-db-name db-name dir)
  (declare (ignore db-dir admin-db-name))
  (mita.db:with-connection (conn (make-instance 'file-db
                                  :db-name db-name
                                  :base-dir dir))
    (let ((hash (make-hash-table :test #'equal)))
      (dolist (name (list +image+
                          +album+
                          +album-images+))
        (setf (gethash name hash) nil))
      (setf (connection-table-hash conn) hash))))

(defun drop-database (admin-db-name db-name dir)
  (declare (ignore admin-db-name))
  (mita.db:with-connection (conn (make-instance 'file-db
                                  :db-name db-name
                                  :base-dir dir))
    (setf (connection-table-hash conn) (make-hash-table :test #'equal))))

(defun select-rows-if (conn name value-pred)
  (remove-if-not value-pred (gethash name (connection-table-hash conn))))

(defun delete-rows-if (conn name value-pred)
  (let ((rows (remove-if value-pred
                         (gethash name (connection-table-hash conn)))))
    (setf (gethash name (connection-table-hash conn)) rows)))


;;;


(defun row->image (row)
  (mita.image:make-image
   :id (mita.id:parse (first row))
   :source (second row)
   :path (third row)))

(defun image->row (i)
  (list (mita.id:to-string (mita.image:image-id i))
        (mita.image:image-source i)
        (mita.image:image-path i)))

(defmethod mita.image:save-images ((conn connection) (images list))
  (alexandria:appendf (gethash +image+ (connection-table-hash conn))
                      (mapcar #'image->row images)))

(defmethod mita.image:load-images-by-ids ((conn connection)
                                          (image-ids list))
  (mapcar #'row->image
          (select-rows-if
           conn
           +image+
           (let ((string-ids (mapcar #'mita.id:to-string image-ids)))
             (lambda (r)
               (member (first r) string-ids :test #'string=))))))

(defmethod mita.image:delete-images ((conn connection)
                                     (image-ids list))
  (delete-rows-if conn
                  +image+
                  (let ((string-ids (mapcar #'mita.id:to-string image-ids)))
                    (lambda (r)
                      (member (first r) string-ids :test #'string=)))))


(defun row->album (r)
  (mita.album:construct-album
   :id (mita.id:parse (first r))
   :name (second r)
   :thumbnail (row->image (third r))))

(defmethod mita.album:create-albums ((conn connection)
                                     (sources list))
  (let ((all-rows (append
                   (mapcar (lambda (s)
                             (list (mita.id:to-string
                                    (mita.album:album-source-id s))
                                   (mita.album:album-source-name s)
                                   (image->row
                                    (mita.album:album-source-thumbnail s))
                                   (local-time:timestamp-to-unix
                                    (mita.album:album-source-created-on s))))
                           sources)
                   (gethash +album+ (connection-table-hash conn)))))
    (setf (gethash +album+ (connection-table-hash conn))
          (sort all-rows #'< :key #'fourth)))
  (mita.album:load-albums-in conn (mapcar #'mita.album:album-source-id
                                          sources)))

(defmethod mita.album:delete-albums ((conn connection)
                                     (album-id-list list))
  (delete-rows-if conn
                  +album+
                  (let ((string-ids (mapcar #'mita.id:to-string
                                            album-id-list)))
                    (lambda (r)
                      (member (first r) string-ids :test #'string=)))))

(defmethod mita.album:load-albums-in ((conn connection)
                                      (album-id-list list))
  (mapcar #'row->album
          (select-rows-if conn
                          +album+
                          (let ((string-ids (mapcar #'mita.id:to-string
                                                    album-id-list)))
                            (lambda (r)
                              (member (first r) string-ids
                                      :test #'string=))))))

(defmethod mita.album:load-albums ((conn connection)
                                   (offset integer)
                                   (limit integer))
  (let* ((rows (gethash +album+ (connection-table-hash conn)))
         (length (length rows)))
    (mapcar #'row->album
            (subseq rows
                    (min offset length)
                    (min (+ offset limit) length)))))
                      
(defmethod mita.album:update-album-images ((conn connection)
                                           (album mita.album:album)
                                           (images list))
  (let ((entry (assoc (mita.id:to-string (mita.album:album-id album))
                      (gethash +album-images+ (connection-table-hash conn))
                      :test #'string=)))
    (if entry
        (setf (cdr entry)
              (mapcar #'image->row images))
        (push (cons (mita.id:to-string (mita.album:album-id album))
                    (mapcar #'image->row images))
              (gethash +album-images+ (connection-table-hash conn))))))

(defmethod mita.album:album-images ((conn connection)
                                    (album mita.album:album))
  (mapcar #'row->image
          (cdr (assoc (mita.id:to-string (mita.album:album-id album))
                      (gethash +album-images+ (connection-table-hash conn))
                      :test #'string=))))
