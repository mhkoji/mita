(defpackage :mita.docker.config
  (:use :cl)
  (:export :*locator*
           :*session-store*
           :*static-root*
           :*content-base*
           :*thumbnail-base*))
(in-package :mita.docker.config)
(ql:quickload '(:mita
                :mita-util-auth))

(setq *read-eval* nil)

(defvar *locator*
  (mita.rdb.mysql:make-locator
   :user "root"
   :host "mysql"
   :port 3306)
  #+nil
  (mita.rdb.postgres:make-locator
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
