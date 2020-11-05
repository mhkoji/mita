(defpackage :mita.web.server
  (:use :cl)
  (:export :start
           :init-db))
(in-package :mita.web.server)

(defvar *connector* (mita.postgres:make-connector
                     :user "postgres"
                     :host "localhost"
                     :port 5432))

(defvar *handler* nil)

(defun system-relative-pathname (name)
  (asdf:system-relative-pathname (asdf:find-system :mita) name))

(defun create-account (postgres-dir connector username password)
  (let ((account
         (mita.postgres:with-admin-gateway (gw connector)
           (mita.account:create-account gw username password))))
    (mita.postgres:create-account-database postgres-dir account connector)
    account))

(defun init-db (&key (postgres-dir
                      (system-relative-pathname "../postgres/"))
                     (connector *connector*)
                     drop-p)
  (mita.postgres:with-admin-gateway (gw connector)
    (declare (ignore gw))
    (when drop-p
      (mapc (lambda (q)
              (postmodern:execute q))
            (list "DROP SCHEMA public CASCADE;"
                  "CREATE SCHEMA public;"
                  "GRANT ALL ON SCHEMA public TO postgres;"
                  "GRANT ALL ON SCHEMA public TO public;"))))
  (mita.postgres:with-admin-gateway (gw connector)
    (declare (ignore gw))
    (mita.postgres:create-account-tables postgres-dir)
    (postmodern:execute-file
     (merge-pathnames postgres-dir "./sessions-ddl.sql")))
  (create-account postgres-dir connector "mita" "mita"))

(defun start (&key (port 5001)
                   (static-root
                    (system-relative-pathname "../mita-web/static/"))
                   (use-thread t)
                   (thumbnail-root)
                   (content-root
                    ;; Call directory-exists-p to resolve symlink beforehand
                    (cl-fad:directory-exists-p
                     (system-relative-pathname "../data/")))
                   (session-store mita.auth.server:*session-store*)
                   (connector *connector*))
  (when *handler*
    (clack:stop *handler*))
  (setq *handler*
        (clack:clackup
         (lack:builder
          (:static :path "/static/" :root static-root)

          (:session :store session-store)

          (mita.web.auth:make-middleware
           :login-url
           mita.web.server.externs:*login-url*

           :permit-list
           (list mita.web.server.externs:*login-url*)

           :is-authenticated-fn
           (lambda (session-holder)
             (mita.auth:is-authenticated-p session-holder connector)))

          (let ((app (make-instance 'ningle:<app>)))
            (mita.web.server.ningle:route-image app connector)
            (mita.web.server.ningle:route-album app connector)
            (mita.web.server.ningle:route-view app connector)
            (mita.web.server.ningle:route-page app connector)
            (mita.web.server.ningle:route-tag app connector)
            (when (and thumbnail-root
                       (cl-fad:directory-exists-p content-root))
              (mita.web.server.ningle:route-dir
               app connector thumbnail-root content-root))
            app))
         :use-thread use-thread
         :port port)))
