(defpackage :mita.web.auth
  (:use :cl)
  (:export :make-middleware))
(in-package :mita.web.auth)

(defun is-authenticated-p (env)
  (let ((session (getf env :lack.session)))
    (gethash mita.web.server.externs:*session-auth-key* session)))

(defun request-permitted-p (permit-list url)
  (dolist (regex permit-list)
    (when (cl-ppcre:scan regex url)
      (return t))))

(defun make-middleware (&key login-url permit-list)
  (lambda (app)
    (lambda (env)
      (if (or (is-authenticated-p env)
              (request-permitted-p permit-list (getf env :path-info)))
          (funcall app env)
          (let ((location
                 (format nil "~A?redirect=~A"
                         login-url
                         (quri:url-encode (getf env :request-uri)))))
            `(300 (:location ,location) nil))))))
