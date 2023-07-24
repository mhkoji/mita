(defpackage :mita.web.service
  (:use :cl)
  (:export :service
           :service-warmup
           :make-service)
  (:import-from :mita.util.threading
                :->
                :->>))
(in-package :mita.web.service)

(defstruct service
  file-store
  (tag-store (mita.tag:make-store
              :dir *default-pathname-defaults*)))

(defmethod mita.web.folder:service-file-store ((service service))
  (service-file-store service))

(defmethod mita.web.folder:service-folder-tags ((service service)
                                                (folder mita.file:file))
  (mita.web.tag:service-content-tags service folder))

(defmethod mita.web.folder:service-folder-set-tags ((service service)
                                                    (folder mita.file:file)
                                                    (tag-id-list list))
  (mita.web.tag:service-content-set-tags service folder tag-id-list))

(defmethod mita.web.tag:service-tag-store ((service service))
  (service-tag-store service))

(defmethod mita.web.tag:service-list-folders ((service service)
                                              (content-id-list list))
  (mita.web.folder:service-list-folders service content-id-list))

;;;

(defun service-warmup (service)
  (bt:make-thread
   (lambda ()
     (ignore-errors
      (mita.file:store-prepare-cache (service-file-store service))))))
