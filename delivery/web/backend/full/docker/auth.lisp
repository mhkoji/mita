(defpackage :mita.docker.auth
  (:use :cl :mita.docker.config)
  (:export :main))
(in-package :mita.docker.auth)
(ql:quickload :mita-delivery-web-full-auth :silent t)

(defun main (&rest argv)
  (declare (ignore argv))
  (mita.delivery.web.full.auth.server:start
   :port 5002
   :session-store *session-store*
   :db-manager *db-manager*
   :static-root *static-root*
   :content-base *content-base*
   :thumbnail-base *thumbnail-base*
   :use-thread nil))
