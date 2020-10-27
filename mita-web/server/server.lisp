(defpackage :mita.web.server
  (:use :cl)
  (:export :start
           :init-db
           :*session-store*))
(in-package :mita.web.server)

(defvar *handler* nil)

(defvar *session-store* (lack.session.store.memory:make-memory-store))

(defvar *connector* (mita.db.postgres:make-connector
                     :database "mita"
                     :user "postgres"
                     :password ""
                     :host "localhost"
                     :port 5432))

(defun system-relative-pathname (name)
  (asdf:system-relative-pathname (asdf:find-system :mita) name))

(defun init-db (&key (connector *connector*)
                     drop-p)
  (mita.db.postgres:with-transaction (db connector)
    (when drop-p
      (mapc (lambda (q)
              (postmodern:execute q))
            (list "DROP SCHEMA public CASCADE;"
                  "CREATE SCHEMA public;"
                  "GRANT ALL ON SCHEMA public TO postgres;"
                  "GRANT ALL ON SCHEMA public TO public;")))
    (mapc (lambda (p)
            (postmodern:execute-file (system-relative-pathname p)))
          (list "./db/postgres-ddl.sql"
                "../mita-account/db-postgres-ddl.sql")))
  (mita:with-gateway (gw connector)
    (mita.account:create-account gw "mita" "mita")))

(defun start (&key (port 5001)
                   (root (system-relative-pathname "../mita-web/"))
                   (init-db nil)
                   (use-thread t)
                   (connector *connector*))
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
