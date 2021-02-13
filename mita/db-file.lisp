(defpackage :mita.db.file
  (:use :cl)
  (:export :make-connector
           :with-admin-db
           :with-db
           :init))
(in-package :mita.db.file)

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

(defun account-table-name (account)
  (let ((id-string (mita.id:to-string
                    (mita.account:account-id account))))
    (format nil "account_~A"
            (string-downcase
             (cl-ppcre:regex-replace-all "-" id-string "_")))))

(defmacro with-admin-db ((db connector) &body body)
  `(with-file-db (,db (connector-dir ,connector))
     ,@body))

(defmacro with-db ((db account connector) &body body)
  `(with-file-db (,db (format nil "~A/~A/"
                              (connector-dir ,connector)
                              (account-table-name ,account)))
     ,@body))

(defvar +image+ "image")
(defvar +album+ "album")
(defvar +album-thumbnail-image+ "album-thumbnail-image")
(defvar +album-image+ "album-image")

(defvar +account+ "account")

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

;;;;

(defmethod mita.account.db:account-insert ((db file-db)
                                           (account mita.account.db:account))
  (push (list (mita.id:to-string
               (mita.account.db:account-id account))
              (mita.account.db:account-username account)
              (mita.account.db:hashed-password-string
               (mita.account.db:account-hashed-password account)))
        (gethash +account+ (table-hash db))))

(labels ((parse-account (row)
           (mita.account.db:make-account
            :id (mita.id:parse (first row))
            :username (second row)
            :hashed-password (mita.account.db:make-hashed-password
                              :string (third row)))))
  (defmethod mita.account.db:account-select ((db file-db)
                                             (username string))
    (car (mapcar
          #'parse-account
          (select-rows-if db +account+
                          (lambda (row)
                            (string= (second row) username))))))

  (defmethod mita.account.db:account-select-by-id ((db file-db)
                                                   (id mita.id:id))
    (car (mapcar
          #'parse-account
          (select-rows-if db +account+
                          (lambda (row)
                            (mita.id:id= (mita.id:parse (first row))
                                         id)))))))

;;;;

(defun create-account-database (connector account)
  (let ((hash (make-hash-table :test #'equal)))
    (dolist (name (list +image+
                        +album+
                        +album-thumbnail-image+
                        +album-image+))
      (setf (gethash name hash) nil))
    (write-table-hash (format nil "~A/~A/"
                              (connector-dir connector)
                              (account-table-name account))
                      hash)))

(defun create-account (connector username password)
  (let ((account
         (with-admin-db (db connector)
           (mita.account:create-account db username password))))
    (create-account-database connector account)
    account))

(defun init (connector)
  (let ((hash (make-hash-table :test #'equal)))
    (dolist (name (list +account+))
      (setf (gethash name hash) nil))
    (write-table-hash (connector-dir connector) hash))
  (create-account connector "mita" "mita"))
