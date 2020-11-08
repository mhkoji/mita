(defpackage :mita.docker.web
  (:use :cl)
  (:export :start
           :start-aserve
           :init))
(in-package :mita.docker.web)
(ql:quickload '(:mita-web
                :mita-web-aserve))

(defvar *connector*
  (mita.postgres:make-connector
   :user "postgres"
   :host "localhost"
   :port 5432))

(defun start (&rest argv)
  (declare (ignore argv))
  (mita.web.server:start
   :port 5001
   :static-root "/app/static/"
   :content-root "/data/albums/"
   :thumbnail-root "/data/thumbnails/"
   :session-store (mita.auth.session:make-redis-store)
   ;; This server halts by broken pippes errors occurred when there is a number of accesses for images.
   ;; Thus we use aserve instead.
   :serve-image nil
   :connector *connector*
   :use-thread nil))

(defun start-aserve (&rest argv)
  (declare (ignore argv))
  (mita.web.server.aserve:start
   :port 5003
   :content-root "/data/albums/"
   :thumbnail-root "/data/thumbnails/"
   :session-store (mita.auth.session:make-redis-store)
   :connector *connector*)
  (loop do (sleep 1000)))

(defun init (&rest argv)
  (declare (ignore argv))
  (mita.web.server:init-db
   :connector (mita.postgres:make-connector
               :user "postgres"
               :host "localhost"
               :port 5432)
   :postgres-dir "/app/postgres/"))
