(defpackage :mita.web.server.clack
  (:use :cl)
  (:export :start
           :init-db))
(in-package :mita.web.server.clack)

(defvar *connector* (mita.db.impl:make-connector))

(defvar *handler* nil)

(defun system-relative-pathname (name)
  (asdf:system-relative-pathname (asdf:find-system :mita) name))

(defun start (&key (port 5001)
                   (static-root
                    (system-relative-pathname "../mita-web/static/"))
                   (use-thread t)
                   (serve-image t)
                   (thumbnail-root
                    ;; Call directory-exists-p to resolve symlink beforehand
                    (cl-fad:directory-exists-p
                     (system-relative-pathname "../data/thumbnails/")))
                   (content-root
                    (cl-fad:directory-exists-p
                     (system-relative-pathname "../data/albums/")))
                   (session-store mita.auth.server:*session-store*)
                   (connector *connector*))
  (when *handler*
    (clack:stop *handler*))
  (setq *handler*
        (clack:clackup
         (lack:builder
          (:static :path "/static/" :root static-root)

          (:session :store session-store)

          (mita.web.server.clack.log:make-middleware)

          (mita.web.server.clack.auth:make-middleware
           :login-url
           mita.web.server.externs:*login-url*

           :permit-list
           (list mita.web.server.externs:*login-url*)

           :is-authenticated-fn
           (lambda (session-holder)
             (mita.auth:is-authenticated-p session-holder connector)))

          (mita.web.server.clack.mita:make-middleware
           connector
           :thumbnail-root thumbnail-root
           :content-root content-root
           :serve-image-p serve-image)

          (lambda (env)
            (declare (ignore env))
            '(302 (:location "/") nil)))
         ;; hunchentoot accepts nil as address, which means the server accepts connections from all IP addresses.
         :address nil
	 ;; Don't have to invoke a debugger. No one can take care of it.
	 ;; setq after clackup because clackup set the var to T.
	 :debug nil
         :use-thread use-thread
         :port port)))
