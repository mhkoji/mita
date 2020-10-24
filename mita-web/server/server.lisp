(defpackage :mita.web.server
  (:use :cl)
  (:export :start
           :init-db
           :*session-store*))
(in-package :mita.web.server)

(defvar *handler* nil)

(defvar *session-store* (lack.session.store.memory:make-memory-store))

(defun system-relative-pathname (name)
  (asdf:system-relative-pathname (asdf:find-system :mita) name))

(defun start (&key (port 5001)
                   (root (system-relative-pathname "../mita-web/"))
                   (init-db nil)
                   (use-thread t)
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
       (system-relative-pathname "./db/postgres-ddl.sql"))
      (postmodern:execute-file
       (system-relative-pathname "../mita-account/db-postgres-ddl.sql")))

    (mita:with-gateway (gw connector)
      (mita.account:create-account gw "mita" "mita")))

  (when *handler*
    (clack:stop *handler*))
  (setq *handler* (clack:clackup
                   (lack:builder
                    (:static :path "/static/"
                             :root (merge-pathnames "static/" root))

                    (:session :store *session-store*)

                    (mita.web.auth:make-middleware
                     :login-url mita.web.server.externs:*login-url*
                     :permit-list (list mita.web.server.externs:*login-url*))

                    (let ((app (make-instance 'ningle:<app>)))
                      (mita.web.server.ningle:route-image app connector)
                      (mita.web.server.ningle:route-album app connector)
                      (mita.web.server.ningle:route-view app connector)
                      (mita.web.server.ningle:route-page app connector)
                      (mita.web.server.ningle:route-tag app connector)
                      app))
                   :use-thread use-thread
                   :port port)))
