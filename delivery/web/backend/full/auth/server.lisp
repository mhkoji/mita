(defpackage :mita.delivery.web.full.auth.server
  (:use :cl)
  (:import-from :alexandria
                :when-let)
  (:export :start
           :*session-store*))
(in-package :mita.delivery.web.full.auth.server)

(defun system-relative-pathname (name)
  (asdf:system-relative-pathname (asdf:find-system :mita) name))

(defvar *session-store* (lack.session.store.memory:make-memory-store))

(defvar *db-manager*
  (make-instance 'mita.auth.admin.db:mysql-manager
                 :db-dir (system-relative-pathname "../mysql/")))

(defvar *handler* nil)

(defun start (&key (port 5002)
                   (session-store *session-store*)
                   (db-manager *db-manager*)
                   (static-root
                    (cl-fad:directory-exists-p
                     (system-relative-pathname
                      "../delivery/web/backend/auth/static/")))
                   (content-base
                    (cl-fad:directory-exists-p
                     (system-relative-pathname "../data/content/")))
                   (thumbnail-base
                    (cl-fad:directory-exists-p
                     (system-relative-pathname "../data/thumbnail/")))
                   (use-thread t))
  (when *handler*
    (clack:stop *handler*))
  (setq *handler* (clack:clackup
                   (lack:builder
                    (:static :path "/auth/static/" :root static-root)

                    (:session :store session-store)

                    (let ((app (make-instance 'ningle:<app>)))
                      (mita.delivery.web.full.auth.ningle:route-auth
                       app db-manager :top-url "/albums")
                       
                      (mita.delivery.web.full.auth.ningle:route-admin
                       app
                       db-manager
                       (namestring content-base)
                       (namestring thumbnail-base))
                      app))
                   :address "0.0.0.0"
                   :use-thread use-thread
		   :debug nil
                   :port port)))