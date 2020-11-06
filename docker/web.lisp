(defpackage :mita.docker.web
  (:use :cl)
  (:export :start :init))
(in-package :mita.docker.web)
(ql:quickload :mita-web :silent t)

(defun start (&rest argv)
  (declare (ignore argv))
  (mita.web.server:start
   :port 5001
   :static-root "/app-output/static/"
   :content-root "/data/albums/"
   :thumbnail-root "/data/thumbnails/"
   :session-store (mita.auth.session:make-redis-store)
   :connector (mita.postgres:make-connector
               :user "postgres"
               :host "localhost"
               :port 5432)
   :use-thread nil))

(defun init (&rest argv)
  (declare (ignore argv))
  (mita.web.server:init-db
   :connector (mita.postgres:make-connector
               :user "postgres"
               :host "localhost"
               :port 5432)
   :postgres-dir "/app/postgres/"))
