(defpackage :mita.web.server
  (:use :cl)
  (:export :start
           :init-db))
(in-package :mita.web.server)

(defvar *handler* nil)

(defun start (&key (port 5000)
                   (root *default-pathname-defaults*)
                   (init-db nil)
                   (connector
                    (mita.db.postgres:make-connector
                     :database "mita"
                     :user "postgres"
                     :password ""
                     :host "localhost"
                     :port 5432)))
  (when init-db
    (mita.db.postgres:with-transaction (db connector)
      (dolist (q (list "DROP SCHEMA public CASCADE;"
                       "CREATE SCHEMA public;"
                       "GRANT ALL ON SCHEMA public TO postgres;"
                       "GRANT ALL ON SCHEMA public TO public;"))
        (postmodern:execute q))
      (postmodern:execute-file
       (asdf:system-relative-pathname (asdf:find-system :mita)
                                      "./mita/db/postgres-ddl.sql"))))
  (when *handler*
    (clack:stop *handler*))
  (setq *handler* (clack:clackup
                   (lack:builder
                    (:static :path "/static/"
                             :root (merge-pathnames "static/" root))
                    (let ((app (make-instance 'ningle:<app>)))
                      (mita.web.server.ningle:route-image app connector)
                      (mita.web.server.ningle:route-album app connector)
                      (mita.web.server.ningle:route-page app connector)
                      app))
                   :port port)))
