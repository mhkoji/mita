(defpackage :mita.delivery.web.full.server
  (:use :cl)
  (:export :start))
(in-package :mita.delivery.web.full.server)

(defvar *handler* nil)

(defclass dep ()
  ((db-manager
    :initarg :db-manager
    :reader dep-db-manager)
   (content-base
    :initarg :content-base
    :reader dep-content-base)
   (thumbnail-base
    :initarg :thumbnail-base
    :reader dep-thumbnail-base)))

(defgeneric request-account-id (req))

(defmethod request-account-id ((req lack.request:request))
  (getf (lack.request:request-env req) :mita.util.auth.identity))

(defmethod mita.web.dep:get-db ((dep dep) req)
  (mita.auth.admin:get-account-db (dep-db-manager dep)
                                  (request-account-id req)))

(defmethod mita.web.dep:get-content-root ((dep dep) req)
  (mita.auth.admin:account-root (dep-content-base dep)
                                (request-account-id req)))

(defmethod mita.web.dep:get-thumbnail-root ((dep dep) req)
  (mita.auth.admin:account-root (dep-thumbnail-base dep)
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
           (make-instance 'dep
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
