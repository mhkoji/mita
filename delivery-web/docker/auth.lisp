(defpackage :mita.docker.auth
  (:use :cl :mita.docker.config)
  (:export :main))
(in-package :mita.docker.auth)
(ql:quickload :mita-auth-backend :silent t)

(defun main (&rest argv)
  (declare (ignore argv))
  (mita.web.auth.server:start
   :port 5002
   :locator *locator*
   :postgres-dir "/root/quicklisp/local-projects/mita/postgres/"
   :static-root *static-root*
   :content-base *content-base*
   :thumbnail-base *thumbnail-base*
   :session-store *session-store*
   :use-thread nil))
