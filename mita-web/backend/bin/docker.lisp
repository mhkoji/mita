(defpackage :mita.web.bin.docker
  (:use :cl)
  (:export :main))
(in-package :mita.web.bin.docker)

(defun main ()
  (setq mita.web.server.hunchentoot:*service*
        (mita.web:make-service
         :file-store (mita.file:make-store :root-path "/file-store/")
         :tag-store (mita.tag:make-store :dir "/tag-store/")))
  (mita.web.server.hunchentoot:warmup)
  (mita.web.server.hunchentoot:start  :document-root "/mita-static/")
  (loop do (sleep 1)))
