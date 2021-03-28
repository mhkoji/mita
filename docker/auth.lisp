(defpackage :mita.docker.auth
  (:use :cl)
  (:export :main))
(in-package :mita.docker.auth)
(ql:quickload :mita-auth-server :silent t)

(defun main (&rest argv)
  (declare (ignore argv))
  (mita.auth.server:start
   :port 5002
   :connector (mita.util.postgres:make-connector
               :user "postgres"
               :host "postgres"
               :port 5432)
   :postgres-dir "/root/quicklisp/local-projects/mita/postgres/"
   :account-content-base "/data/accounts/"
   :static-root "/app/static/"
   :session-store (mita.util.auth.session:make-redis-store :host "redis")
   :use-thread nil))
