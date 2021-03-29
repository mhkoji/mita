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
                   (connector
                    (mita.db.impl:make-connector))
                   (static-root
                    (cl-fad:directory-exists-p
                     (system-relative-pathname
                      "../mita-admin/auth/static/")))
                   (session-store *session-store*)
                   (postgres-dir
                    (cl-fad:directory-exists-p
                     (system-relative-pathname "../postgres/")))
                   (account-content-base
                    (cl-fad:directory-exists-p
                     (system-relative-pathname "../data/accounts")))
                   (use-thread t))
  (when *handler*
    (clack:stop *handler*))
  (setq *handler* (clack:clackup
                   (lack:builder
                    (:static :path "/auth/static/" :root static-root)

                    (:session :store session-store)

                    (let ((app (make-instance 'ningle:<app>)))
                      (mita.auth.server.ningle:route-auth
                       app connector :top-url "/albums")
                      (mita.admin.server.ningle:route-admin
                       app connector postgres-dir account-content-base)
                      app))
                   :address nil
                   :use-thread use-thread
		   :debug nil
                   :port port)))
