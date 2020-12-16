(defpackage :mita.web.server.clack.auth
  (:use :cl)
  (:export :make-middleware)
  (:import-from :alexandria
                :if-let))
(in-package :mita.web.server.clack.auth)

(defun make-middleware (&key login-url permit-list is-authenticated-fn)
  (labels ((request-permitted-p (url)
             (dolist (regex permit-list)
               (when (cl-ppcre:scan regex url)
                 (return t)))))
    (lambda (app)
      (lambda (env)
        (if-let ((account (funcall
                           is-authenticated-fn
                           (make-instance
                            'mita.auth:lack-session-holder :env env))))
          (funcall app (list* :mita.account account env))
          (if (request-permitted-p (getf env :path-info))
              (funcall app env)
              (let ((location
                     (format nil "~A?redirect=~A"
                             login-url
                             (quri:url-encode (getf env :request-uri)))))
                `(302 (:location ,location) nil))))))))
