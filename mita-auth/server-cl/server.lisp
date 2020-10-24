(defpackage :mita.auth.server
  (:use :cl)
  (:import-from :alexandria
                :when-let)
  (:export :start))
(in-package :mita.auth.server)

(defvar *handler* nil)


(defun system-relative-pathname (name)
  (asdf:system-relative-pathname (asdf:find-system :mita) name))


(defun login-page ()
  (cl-who:with-html-output-to-string (s nil :prologue t)
    (:head
     (:meta :charset "utf-8")
     (:title "mita")
     (:link :rel "stylesheet"
            :href "/auth/static/gen/login.bundle.css"))
    (:body
     (:div :id "app")
     (:script
      :type "text/javascript"
      :src "/auth/static/gen/login.bundle.js"))))

(defun is-allowed (connector request)
  (let ((body (lack.request:request-body-parameters request)))
    (when-let ((username (cdr (assoc "username" body :test #'string=)))
               (password (cdr (assoc "password" body :test #'string=))))
      (mita:with-gateway (gw connector)
        (mita.account:find-account gw username password)))))


(defun start (&key (port 5002)
                   (root (system-relative-pathname "../mita-auth/"))
                   (use-thread t)
                   (connector
                    (mita.db.postgres:make-connector
                     :database "mita"
                     :user "postgres"
                     :password ""
                     :host "localhost"
                     :port 5432)))
  (when *handler*
    (clack:stop *handler*))
  (setq *handler* (clack:clackup
                   (lack:builder
                    (:static :path "/auth/static/"
                             :root (merge-pathnames "static/" root))

                    (:session :store mita.web.server:*session-store*)

                    (let ((app (make-instance 'ningle:<app>)))
                      (mita.auth.ningle:route-auth app
                       :top-url "/albums"
                       :login-page-fn #'login-page
                       :is-allowed-fn (lambda ()
                                        (is-allowed connector
                                                    ningle:*request*)))
                      app))
                   :use-thread use-thread
                   :port port)))
