(defpackage :mita.docker.mita-web
  (:use :cl)
  (:export :main))
(in-package :mita.docker.mita-web)

(defun main ()
  (setq mita.web.server.hunchentoot:*service*
        (mita.web:make-service
         :file-store (mita.file:make-store :root-path "/file-root-path/")
         :tag-store (mita.tag:make-store :dir "/tag-store/")))
  (mita.web.server.hunchentoot:warmup)
  (mita.web.server.hunchentoot:start  :document-root "/mita-www/")
  (loop do (sleep 1)))
