(defpackage :mita.web.bin.docker
  (:use :cl)
  (:export :main))
(in-package :mita.web.bin.docker)

(defun main ()
  (let ((service
         (mita.web:make-service
          :file-store (mita.file:make-store :root-path "/file-store/")
          :tag-store (mita.tag:make-store :dir "/tag-store/"))))

    (mita.web.server.hunchentoot:warmup service)

    (let ((thread (mita.web.server.hunchentoot:start
                   :service service
                   :document-root "/mita-www/")))
      (bt:join-thread thread))))
