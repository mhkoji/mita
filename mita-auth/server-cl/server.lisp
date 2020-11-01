(defpackage :mita.auth.server
  (:use :cl)
  (:import-from :alexandria
                :when-let)
  (:export :start
           :*session-store*))
(in-package :mita.auth.server)

(defvar *handler* nil)

(defvar *session-store* (lack.session.store.memory:make-memory-store))

(defun system-relative-pathname (name)
  (asdf:system-relative-pathname (asdf:find-system :mita) name))

(defun start (&key (port 5002)
                   (root (system-relative-pathname "../mita-auth/"))
                   (use-thread t)
                   (connector
                    (mita.postgres:make-connector
                     :user "postgres"
                     :host "localhost"
                     :port 5432)))
  (when *handler*
    (clack:stop *handler*))
  (setq *handler* (clack:clackup
                   (lack:builder
                    (:static :path "/auth/static/"
                             :root (merge-pathnames "static/" root))

                    (:session :store *session-store*)

                    (let ((app (make-instance 'ningle:<app>)))
                      (mita.auth.ningle:route-auth
                       app connector :top-url "/albums")
                      app))
                   :use-thread use-thread
                   :port port)))
