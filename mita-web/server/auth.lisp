(defpackage :mita.web.auth
  (:use :cl)
  (:export :authenticator
           :authenticate
           :is-allowed
           :is-authenticated-p
           :make-middleware))
(in-package :mita.web.auth)

(defclass authenticator () ())

(defgeneric is-allowed (authenticator identifier credential)
  (:method (authenticaor identifier credential)
    nil))

(defstruct configure
  authenticator
  login-url
  permit-list)

(defun request-permited-p (conf url)
  (dolist (regex (configure-permit-list conf))
    (when (cl-ppcre:scan regex url)
      (return t))))

(defun is-authenticated-p (session)
  (gethash "mita.app.auth:is-authenticated-p" session))

(defun authenticate (session authenticator identifier credential)
  (when t (is-allowed authenticator identifier credential)
    (setf (gethash "mita.app.auth:is-authenticated-p" session) t)
    t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-middleware (&key login-url permit-list)
  (let ((conf (make-configure
               :login-url login-url
               :permit-list permit-list)))
    (lambda (app)
      (lambda (env)
        (if (or (request-permited-p conf (getf env :path-info))
                (is-authenticated-p (getf env :lack.session)))
            (funcall app env)
            (let ((location
                   (format nil "~A?redirect=~A"
                           (configure-login-url conf)
                           (quri:url-encode (getf env :request-uri)))))
              `(300 (:location ,location) nil)))))))
