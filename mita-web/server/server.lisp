(defpackage :mita.web.server
  (:use :cl)
  (:export :server
           :serve-image
           :request-account)
  (:import-from :alexandria
                :when-let*))
(in-package :mita.web.server)

(defclass server ()
  ((connector
    :initarg :connector
    :reader server-connector)
   (content-root
    :initarg :content-root
    :reader server-content-root)
   (thumbnail-root
    :initarg :thumbnail-root
    :reader server-thumbnail-root)))

;; App-independent getter(s)
(defgeneric request-account (req))


(defmacro with-db ((db server req) &body body)
  `(mita.postgres:with-db (,db (request-account ,req)
                               (server-connector ,server))
     ,@body))

(defun serve-image (server req image-id-string
                    &key on-found
                         on-not-found)
  (with-db (db server req)
    (let ((image-id (mita.id:parse-short-or-nil image-id-string)))
      (unless image-id
        (return-from serve-image (funcall on-not-found)))
      (let ((image (mita.image:load-image db image-id)))
        (unless image
          (return-from serve-image (funcall on-not-found)))
        (let ((root (cadr (assoc
                           (mita.image:image-source image)
                           (list (list mita.image:+source-content+
                                       (server-content-root server))
                                 (list mita.image:+source-thumbnail+
                                       (server-thumbnail-root server)))))))
          (unless root
            (return-from serve-image (funcall on-not-found)))
          (funcall on-found
                   (parse-namestring
                    (format nil "~A/~A"
                            root
                            (mita.image:image-path image)))))))))
