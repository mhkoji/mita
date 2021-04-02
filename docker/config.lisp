(defpackage :mita.docker.config
  (:use :cl)
  (:export :*connector*
           :*session-store*
           :*static-root*
           :*content-base*
           :*thumbnail-base*))
(in-package :mita.docker.config)
(ql:quickload '(:mita-util-postgres
                :mita-util-auth))

(defvar *connector*
  (mita.util.postgres:make-connector
   :user "postgres"
   :host "postgres"
   :port 5432))

(defvar *session-store*
  (mita.util.auth.session:make-redis-store
   :host "redis"))

(defvar *static-root*
  "/app/static/")

(defvar *content-base*
  "/data/content/")

(defvar *thumbnail-base*
  "/data/thumbnail/")
