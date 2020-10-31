(defpackage :mita.web.server
  (:use :cl)
  (:export :start
           :init-db
           :*session-store*))
(in-package :mita.web.server)

(defvar *connector* (mita.postgres:make-connector
                     :user "postgres"
                     :host "localhost"
                     :port 5432))

(defvar *handler* nil)

(defvar *session-store* (lack.session.store.memory:make-memory-store))

(defun system-relative-pathname (name)
  (asdf:system-relative-pathname (asdf:find-system :mita) name))

(defun create-account (connector username password)
  (let ((account
         (mita.postgres:with-admin-gateway (gw connector)
           (mita.account:create-account gw username password)
           (mita.account:find-account gw username))))
    (mita.postgres:create-account-database account connector)
    account))

(defun init-db (&key (connector *connector*)
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
    (mita.account.postgres:create-tables))
  (create-account connector "mita" "mita"))

(defun start (&key (port 5001)
                   (root (system-relative-pathname "../mita-web/"))
                   (use-thread t)
                   (connector *connector*))
  (when *handler*
    (clack:stop *handler*))
  (setq *handler* (clack:clackup
                   (lack:builder
                    (:static :path "/static/"
                             :root (merge-pathnames "static/" root))

                    (:session :store *session-store*)

                    #+nil
                    (mita.web.auth:make-middleware
                     :login-url mita.web.server.externs:*login-url*
                     :permit-list (list mita.web.server.externs:*login-url*))

                    ;; TODO: set account from session data
                    (lambda (app)
                      (lambda (env)
                        (mita.postgres:with-admin-gateway (gw connector)
                          (let ((account
                                 (mita.account:find-account gw "mita")))
                            (funcall app
                                     (list* :mita.account account env))))))

                    (let ((app (make-instance 'ningle:<app>)))
                      (mita.web.server.ningle:route-image app connector)
                      (mita.web.server.ningle:route-album app connector)
                      (mita.web.server.ningle:route-view app connector)
                      (mita.web.server.ningle:route-page app connector)
                      (mita.web.server.ningle:route-tag app connector)
                      app))
                   :use-thread use-thread
                   :port port)))
