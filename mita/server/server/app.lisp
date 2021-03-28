(defpackage :mita.server.app
  (:use :cl)
  (:export :request-account-id
           :get-connector
           :with-db
           :with-db-spec
           :spec
           :image-serve
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
   (account-content-base
    :initarg :account-content-base
    :reader spec-account-content-base)
   (thumbnail-root
    :initarg :thumbnail-root
    :reader spec-thumbnail-root)))

(defmacro with-db-spec ((db spec rec) &body body)
  `(with-db (,db (spec-connector ,spec) ,rec)
     ,@body))

(defun account-content-root (spec req)
  (mita.account:account-content-root
   (request-account-id req)
   (namestring (spec-account-content-base spec))))

;;;

(defun dir-serve (spec req path &key on-found on-not-found)
  (when (not path)
    (return-from dir-serve (funcall on-not-found)))
  (let* ((content-root (account-content-root spec req))
         (full-path (parse-namestring
                     (concatenate 'string content-root "/" path))))
    (when (not (cl-fad:file-exists-p full-path))
      (return-from dir-serve (funcall on-not-found)))
    (funcall on-found (mita.dir:as-file content-root full-path))))

(defun dir-add-albums (spec req path)
  (let* ((content-root (account-content-root spec req))
         (full-path (parse-namestring
                     (concatenate 'string content-root "/" path))))
    (when (not (cl-fad:file-exists-p full-path))
      (return-from dir-add-albums))
    (let ((dirs (mita.dir:list-dirs content-root full-path)))
      (with-db-spec (db spec req)
        (mita.add-albums:run db dirs (spec-thumbnail-root spec))))))

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
        (let ((root (cadr (assoc
                           (mita.image:image-source image)
                           (list (list mita.image:+source-content+
                                       (account-content-root spec req))
                                 (list mita.image:+source-thumbnail+
                                       (spec-thumbnail-root spec)))))))
          (unless root
            (return-from image-serve (funcall on-not-found)))
          (funcall on-found
                   (parse-namestring
                    (format nil "~A/~A"
                            root
                            (mita.image:image-path image)))))))))
