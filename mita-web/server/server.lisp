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

(defun init-db (&key (drop-p nil)
                     (connector *connector*)
                     (postgres-dir
                      (system-relative-pathname "../postgres/")))
  (mita.postgres:init postgres-dir connector drop-p))

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

          (mita.web.auth:make-middleware
           :login-url
           mita.web.server.externs:*login-url*

           :permit-list
           (list mita.web.server.externs:*login-url*)

           :is-authenticated-fn
           (lambda (session-holder)
             (mita.auth:is-authenticated-p session-holder connector)))

          (let ((app (make-instance 'ningle:<app>)))
            (mita.web.server.ningle:route-album app connector)
            (mita.web.server.ningle:route-view app connector)
            (mita.web.server.ningle:route-page app connector)
            (mita.web.server.ningle:route-tag app connector)
            (mita.web.server.ningle:route-home app)
            (when serve-image
              (mita.web.server.ningle:route-image
               app connector thumbnail-root content-root))
            (mita.web.server.ningle:route-dir
             app connector thumbnail-root content-root)
            app))
         ;; hunchentoot accepts nil as address, which means the server accepts connections from all IP addresses.
         :address nil
	 ;; Don't have to invoke a debugger. No one can take care of it.
	 ;; setq after clackup because clackup set the var to T.
	 :debug nil
         :use-thread use-thread
         :port port)))
