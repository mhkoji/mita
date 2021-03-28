(defpackage :mita.server.app
  (:use :cl)
  (:export :request-account-id
           :get-connector
           :with-db
           :with-db-spec
           :spec
           :serve-image)
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
  (concatenate 'string
   (namestring (spec-account-content-base spec))
   (mita.account::account-id->db-name
    (mita.server.app:request-account-id req))
   "/"))

(defun serve-image (spec req image-id-string
                    &key on-found
                         on-not-found)
  (with-db-spec (db spec req)
    (let ((image-id (mita.id:parse-short-or-nil image-id-string)))
      (unless image-id
        (return-from serve-image (funcall on-not-found)))
      (let ((image (mita.image:load-image db image-id)))
        (unless image
          (return-from serve-image (funcall on-not-found)))
        (let ((root (cadr (assoc
                           (mita.image:image-source image)
                           (list (list mita.image:+source-content+
                                       (account-content-root spec req))
                                 (list mita.image:+source-thumbnail+
                                       (spec-thumbnail-root spec)))))))
          (unless root
            (return-from serve-image (funcall on-not-found)))
          (funcall on-found
                   (parse-namestring
                    (format nil "~A/~A"
                            root
                            (mita.image:image-path image)))))))))
