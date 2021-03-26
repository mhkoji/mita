(defpackage :mita.util.auth.lack
  (:use :cl)
  (:export :authenticate
           :pass-or-deny
   	   :lack-session-holder)
  (:import-from :alexandria
                :if-let))
(in-package :mita.util.auth.lack)

(defclass lack-session-holder (mita.util.auth:session-holder)
  ((env :initarg :env)))

(defmethod mita.util.auth:get-session ((holder lack-session-holder))
  (with-slots (env) holder
    (getf env :lack.session)))

(defmethod mita.util.auth:renew-session-id ((holder lack-session-holder))
  (with-slots (env) holder
    (symbol-macrolet ((options (getf env :lack.session.options)))
      (setf (getf options :change-id) t))))

(defun authenticate ()
  (lambda (app)
    (lambda (env)
      (let ((identity (mita.util.auth:is-authenticated-p
                       (make-instance 'lack-session-holder :env env))))
        (funcall app (list* :mita.util.auth.identity identity env))))))


(defun pass-or-deny (&key login-url permit-list)
  (labels ((request-permitted-p (url)
             (dolist (regex permit-list)
               (when (cl-ppcre:scan regex url)
                 (return t)))))
    (lambda (app)
      (lambda (env)
        (if (or (getf env :mita.util.auth.identity)
                (request-permitted-p (getf env :path-info)))
            (funcall app env)
            (let ((location
                   (format nil "~A?redirect=~A"
                           login-url
                           (quri:url-encode (getf env :request-uri)))))
              `(302 (:location ,location) nil)))))))
