(defpackage :mita.server.app
  (:use :cl)
  (:export :request-account-id
           :get-connector
           :with-db
           :with-db-spec
           :spec
           :image-serve
           :album-upload
           :dir-serve
           :dir-add-albums)
  (:import-from :alexandria
                :when-let*))
(in-package :mita.server.app)

(defgeneric request-account-id (request))

(defmacro with-db ((db conn req) &body body)
  `(mita.account:with-db (,db (request-account-id ,req) ,conn)
     ,@body))

;;;;;
  
(defclass spec ()
  ((connector
    :initarg :connector
    :reader spec-connector)
   (content-base
    :initarg :content-base
    :reader spec-content-base)
   (thumbnail-base
    :initarg :thumbnail-base
    :reader spec-thumbnail-base)))

(defmacro with-db-spec ((db spec rec) &body body)
  `(with-db (,db (spec-connector ,spec) ,rec)
     ,@body))

(defun account-content-root (spec req)
  (mita.account:account-root (spec-content-base spec)
                             (request-account-id req)))

(defun account-thumbnail-root (spec req)
  (mita.account:account-root (spec-thumbnail-base spec)
                             (request-account-id req)))

(defun album-upload (spec req files)
  (let ((full-paths nil)
        (content-root (account-content-root spec req)))
    (loop for (path stream) in files do
      (let ((full-path (concatenate 'string content-root path)))
        (ensure-directories-exist full-path)
        (with-open-file (out full-path
                             :direction :output
                             :element-type '(unsigned-byte 8))
          (alexandria:copy-stream stream out))
        (push full-path full-paths)))
    (let ((dir-paths (remove-duplicates
                      (mapcar (lambda (p)
                                (namestring
                                 (cl-fad:pathname-directory-pathname p)))
                              full-paths)
                      :test #'string=)))
      (let ((dirs (mapcar (lambda (dp)
                            (mita.fs.dir:as-file content-root dp))
                          dir-paths))
            (thumbnail-dir (mita.fs.dir:as-file
                            (account-thumbnail-root spec req)
                            (account-thumbnail-root spec req))))
        (with-db-spec (db spec req)
          (mita.add-albums:run db dirs thumbnail-dir))))))

;;;

(defun dir-serve (spec req path &key on-found on-not-found)
  (when (not path)
    (return-from dir-serve (funcall on-not-found)))
  (let* ((content-root (account-content-root spec req))
         (full-path (parse-namestring
                     (concatenate 'string content-root "/" path))))
    (when (not (cl-fad:file-exists-p full-path))
      (return-from dir-serve (funcall on-not-found)))
    (funcall on-found (mita.fs.dir:as-file content-root full-path))))

(defun dir-add-albums (spec req path)
  (let* ((content-root (account-content-root spec req))
         (full-path (parse-namestring
                     (concatenate 'string content-root "/" path))))
    (when (not (cl-fad:file-exists-p full-path))
      (return-from dir-add-albums))
    (let ((dirs (mita.fs.dir:list-dirs content-root full-path))
          (thumbnail-dir (mita.fs.dir:as-file
                          (account-thumbnail-root spec req)
                          (account-thumbnail-root spec req))))
      (with-db-spec (db spec req)
        (mita.add-albums:run db dirs thumbnail-dir)))))

;;;

(defun image-serve (spec req image-id-string
                    &key on-found
                         on-not-found)
  (with-db-spec (db spec req)
    (let ((image-id (mita.id:parse-short-or-nil image-id-string)))
      (unless image-id
        (return-from image-serve (funcall on-not-found)))
      (let ((image (mita.image:load-image db image-id)))
        (unless image
          (return-from image-serve (funcall on-not-found)))
        (let ((root (cadr
                     (assoc
                      (mita.image:image-source image)
                      (list (list mita.image:+source-content+
                                  (account-content-root spec req))
                            (list mita.image:+source-thumbnail+
                                  (account-thumbnail-root spec req)))))))
          (unless root
            (return-from image-serve (funcall on-not-found)))
          (funcall on-found
                   (parse-namestring
                    (format nil "~A/~A"
                            root
                            (mita.image:image-path image)))))))))
