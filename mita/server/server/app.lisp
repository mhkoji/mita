(defpackage :mita.server.app
  (:use :cl)
  (:export :request-account-id
           :get-connector
           :with-db
           :spec
           :serve-image)
  (:import-from :alexandria
                :when-let*))
(in-package :mita.server.app)

(defgeneric request-account-id (request))

(defgeneric get-connector (conn-holder))

(defmethod get-connector ((conn mita.util.postgres:connector))
  conn)

(defmacro with-db ((db conn-holder req) &body body)
  `(mita.db.impl:with-db (,db (request-account-id ,req)
                              (get-connector ,conn-holder))
     ,@body))


;;;;;
  
(defclass spec ()
  ((connector
    :initarg :connector
    :reader spec-connector)
   (content-root
    :initarg :content-root
    :reader spec-content-root)
   (thumbnail-root
    :initarg :thumbnail-root
    :reader spec-thumbnail-root)))

(defmethod get-connector ((spec spec))
  (spec-connector spec))

(defun serve-image (spec req image-id-string
                    &key on-found
                         on-not-found)
  (with-db (db spec req)
    (let ((image-id (mita.id:parse-short-or-nil image-id-string)))
      (unless image-id
        (return-from serve-image (funcall on-not-found)))
      (let ((image (mita.image:load-image db image-id)))
        (unless image
          (return-from serve-image (funcall on-not-found)))
        (let ((root (cadr (assoc
                           (mita.image:image-source image)
                           (list (list mita.image:+source-content+
                                       (spec-content-root spec))
                                 (list mita.image:+source-thumbnail+
                                       (spec-thumbnail-root spec)))))))
          (unless root
            (return-from serve-image (funcall on-not-found)))
          (funcall on-found
                   (parse-namestring
                    (format nil "~A/~A"
                            root
                            (mita.image:image-path image)))))))))
