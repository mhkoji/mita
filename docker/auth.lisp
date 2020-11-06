(defpackage :mita.docker.auth
  (:use :cl)
  (:export :main))
(in-package :mita.docker.auth)
(ql:quickload :mita-auth :silent t)

(defun main (&rest argv)
  (declare (ignore argv))
  (mita.auth.server:start
   :port 5002
   :static-root "/app-output/static/"
   :session-store (mita.auth.session:make-redis-store)
   :connector (mita.postgres:make-connector
               :user "postgres"
               :host "localhost"
               :port 5432)
   :use-thread nil))
