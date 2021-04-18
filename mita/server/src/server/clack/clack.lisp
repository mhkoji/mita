(defpackage :mita.server.clack
  (:use :cl)
  (:export :start))
(in-package :mita.server.clack)

(defvar *locator* (mita.db.impl:make-locator))

(defvar *handler* nil)

(defun system-relative-pathname (name)
  (asdf:system-relative-pathname (asdf:find-system :mita) name))

(defun start (&key (port 5001)
                   (static-root
                    (system-relative-pathname "../static/"))
                   (use-thread t)
                   (serve-image t)
                   ;; Call directory-exists-p to resolve symlink beforehand
                   (content-base
                    (cl-fad:directory-exists-p
                     (system-relative-pathname "../../data/content/")))
                   (thumbnail-base
                    (cl-fad:directory-exists-p
                     (system-relative-pathname "../../data/thumbnail/")))
                   (session-store mita.auth.server:*session-store*)
                   (locator *locator*))
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
           mita.server.externs:*login-url*
           :permit-list
           (list mita.server.externs:*login-url*))

          (mita.server.clack.mita:make-middleware
           locator
           (make-instance 'mita.server.app:spec
                          :locator locator
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
