(defpackage :mita.web.auth
  (:use :cl)
  (:export :is-allowed
           :is-authenticated-p
           :make-middleware))
(in-package :mita.web.auth)

(defun request-permitted-p (permit-list url)
  (dolist (regex permit-list)
    (when (cl-ppcre:scan regex url)
      (return t))))

(defun is-authenticated-p (session)
  (gethash "mita.app.auth:is-authenticated-p" session))

(defun authenticate (session is-allowed-fn)
  (when (funcall is-allowed-fn)
    (setf (gethash "mita.app.auth:is-authenticated-p" session) t)
    t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-middleware (&key login-url permit-list)
  (lambda (app)
    (lambda (env)
      (if (or (is-authenticated-p (getf env :lack.session))
              (request-permitted-p permit-list (getf env :path-info)))
          (funcall app env)
          (let ((location
                 (format nil "~A?redirect=~A"
                         login-url
                         (quri:url-encode (getf env :request-uri)))))
            `(300 (:location ,location) nil))))))
