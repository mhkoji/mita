(defpackage :mita.web.service
  (:use :cl)
  (:export :service
           :service-warmup
           :make-service))
(in-package :mita.web.service)

(defstruct service
  file-store
  (tag-store (mita.tag:make-store
              :dir *default-pathname-defaults*)))

(defmethod mita.web.folder:service-file-store ((service service))
  (service-file-store service))

;;;

(defmethod mita.web.tag:service-tag-store ((service service))
  (service-tag-store service))

;;;

(defmethod mita.tag:content-id ((c mita.file:folder))
  (namestring (mita.file:file-path c)))

(defmethod mita.tag:content-type ((c mita.file:folder))
  "folder")

(defmethod mita.web.folder:service-get-tags ((service service)
                                             (folder mita.file:file))
  (mita.web.tag:service-content-tags service folder))

(defmethod mita.web.folder:service-set-tags ((service service)
                                             (folder mita.file:file)
                                             (tag-id-list list))
  (mita.web.tag:service-content-set-tags service folder tag-id-list))

(defmethod mita.web.folder:service-tag-namestring-list ((service service)
                                                        (tag-id string))
  (mita.web.tag:service-tag-content-id-list service tag-id "folder"))

;;;

(defun service-warmup (service)
  (bt:make-thread
   (lambda ()
     (ignore-errors
      (mita.file:store-prepare-cache (service-file-store service))))))
