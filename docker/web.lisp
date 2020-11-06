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
   :session-store
   (lack.session.store.dbi:make-dbi-store
    :connector (lambda ()
                 ;; Even though cl-dbi:connect-cached is preferred as in https://github.com/fukamachi/lack/pull/30#issuecomment-418573441,
                 ;; we use cl-dbi:connect and cl-dbi:disconnect because cl-dbi:connect-cached raises a multiple-thread-releated error.
                 ;; To do this the latest versions of lack and cl-dbi is required.
                 (cl-dbi:connect
                  :postgres
                  :database-name "admin"
                  :host "localhost"
                  :port 5432
                  :username "postgres"
                  :password ""))
    :disconnector #'cl-dbi:disconnect)
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
