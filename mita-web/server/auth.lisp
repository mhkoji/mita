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

(defun request-permitted-p (permit-list url)
  (dolist (regex permit-list)
    (when (cl-ppcre:scan regex url)
      (return t))))

(defun is-authenticated-p (session)
  (gethash "mita.app.auth:is-authenticated-p" session))

(defun authenticate (session authenticator identifier credential)
  (when (is-allowed authenticator identifier credential)
    (setf (gethash "mita.app.auth:is-authenticated-p" session) t)
    t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-middleware (&key login-url permit-list)
  (lambda (app)
    (lambda (env)
      (if (or (request-permitted-p permit-list (getf env :path-info))
              (is-authenticated-p (getf env :lack.session)))
          (funcall app env)
          (let ((location
                 (format nil "~A?redirect=~A"
                         login-url
                         (quri:url-encode (getf env :request-uri)))))
            `(300 (:location ,location) nil))))))
