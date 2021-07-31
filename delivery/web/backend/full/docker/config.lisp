(defpackage :mita.docker.config
  (:use :cl)
  (:export :*db-manager*
           :*session-store*
           :*static-root*
           :*content-base*
           :*thumbnail-base*))
(in-package :mita.docker.config)
(ql:quickload '(:mita
                :mita-auth))

(setq *read-eval* nil)

(defvar *db-manager*
  (make-instance 'mita.auth.admin.db:mysql-manager
                 :locator (mita.db.vendor.mysql:make-locator
                           :user "root"
                           :host "mysql"
                           :port 3306)
                 :db-dir "/root/quicklisp/local-projects/mita/mysql/")
  #+nil
  (make-instance 'mita.auth.admin.db:postgres-manager
                 :locator (mita.db.vendor.postgres:make-locator
                           :user "postgres"
                           :host "postgres"
                           :port 5432)
                 :db-dir "/root/quicklisp/local-projects/mita/postgres/"))

(defvar *session-store*
  (mita.util.auth.session:make-redis-store
   :host "redis"))

(defvar *static-root*
  "/app/static/")

(defvar *content-base*
  "/data/content/")

(defvar *thumbnail-base*
  "/data/thumbnail/")
