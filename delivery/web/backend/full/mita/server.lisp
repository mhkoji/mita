(defpackage :mita.delivery.web.full.server
  (:use :cl)
  (:export :start))
(in-package :mita.delivery.web.full.server)

(defvar *handler* nil)

(defclass spec ()
  ((db-manager
    :initarg :db-manager
    :reader spec-db-manager)
   (content-base
    :initarg :content-base
    :reader spec-content-base)
   (thumbnail-base
    :initarg :thumbnail-base
    :reader spec-thumbnail-base)))

(defgeneric request-account-id (req))

(defmethod request-account-id ((req lack.request:request))
  (getf (lack.request:request-env req) :mita.util.auth.identity))

(defmethod mita.web.app:get-db ((spec spec) req)
  (mita.auth.admin:get-account-db (spec-db-manager spec)
                                  (request-account-id req)))

(defmethod mita.web.app:get-content-root ((spec spec) req)
  (mita.auth.admin:account-root (spec-content-base spec)
                                (request-account-id req)))

(defmethod mita.web.app:get-thumbnail-root ((spec spec) req)
  (mita.auth.admin:account-root (spec-thumbnail-base spec)
                                (request-account-id req)))

(defun system-relative-pathname (name)
  (asdf:system-relative-pathname (asdf:find-system :mita) name))

(defun start (&key (port 5001)
                   (session-store
                    mita.delivery.web.full.auth.server:*session-store*)
                   (db-manager
                    mita.delivery.web.full.auth.server::*db-manager*)
                   (static-root
                    (system-relative-pathname
                     "../delivery/web/backend/mita/static/"))
                    ;; Call directory-exists-p to resolve symlink beforehand
                   (content-base
                    (cl-fad:directory-exists-p
                     (system-relative-pathname "../data/content/")))
                   (thumbnail-base
                    (cl-fad:directory-exists-p
                     (system-relative-pathname "../data/thumbnail/")))
                   (use-thread t)
                   (serve-image t))
  (when *handler*
    (clack:stop *handler*))
  (setq *handler*
        (clack:clackup
         (lack:builder
          (:static :path "/static/" :root static-root)

          (:session :store session-store)

          (mita.util.clack:middleware-log)

	  (mita.util.auth.lack:authenticate)

          (mita.util.auth.lack:pass-or-deny
           :login-url
           mita.delivery.web.full.server.externs:*login-url*
           :permit-list
           (list mita.delivery.web.full.server.externs:*login-url*))

          (mita.web.clack:make-middleware
           (make-instance 'spec
                          :db-manager db-manager
                          :content-base (namestring content-base)
                          :thumbnail-base (namestring thumbnail-base))
           :serve-image-p serve-image)

          (lambda (env)
            (declare (ignore env))
            '(302 (:location "/") nil)))
         :address "0.0.0.0"
	 ;; Don't have to invoke a debugger. No one can take care of it.
	 ;; setq after clackup because clackup set the var to T.
	 :debug nil
         :use-thread use-thread
         :port port)))
