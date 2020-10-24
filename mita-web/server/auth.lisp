(defpackage :mita.web.auth
  (:use :cl)
  (:export :session-holder
           :lack-session-holder
           :get-session
           :renew-session-id
           :is-allowed
           :is-authenticated-p
           :authenticate
           :make-middleware))
(in-package :mita.web.auth)

(defclass session-holder () ())

(defgeneric get-session (holder))

(defgeneric renew-session-id (holder))

(defun is-authenticated-p (session-holder)
  (let ((session (get-session session-holder)))
    (gethash "mita.app.auth:is-authenticated-p" session)))

(defun authenticate (session-holder is-allowed-fn)
  (when (funcall is-allowed-fn)
    (let ((session (get-session session-holder)))
      (setf (gethash "mita.app.auth:is-authenticated-p" session) t))
    (renew-session-id session-holder)
    t))


(defun request-permitted-p (permit-list url)
  (dolist (regex permit-list)
    (when (cl-ppcre:scan regex url)
      (return t))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass lack-session-holder (session-holder)
  ((env :initarg :env)))

(defmethod get-session ((holder lack-session-holder))
  (with-slots (env) holder
    (getf env :lack.session)))

(defmethod renew-session-id ((holder lack-session-holder))
  (with-slots (env) holder
    (symbol-macrolet ((options (getf env :lack.session.options)))
      (setf (getf options :change-id) t))))

(defun make-middleware (&key login-url permit-list)
  (lambda (app)
    (lambda (env)
      (if (or (is-authenticated-p
               (make-instance 'lack-session-holder :env env))
              (request-permitted-p permit-list (getf env :path-info)))
          (funcall app env)
          (let ((location
                 (format nil "~A?redirect=~A"
                         login-url
                         (quri:url-encode (getf env :request-uri)))))
            `(300 (:location ,location) nil))))))
