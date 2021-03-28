(defpackage :mita.auth.server.ningle
  (:use :cl)
  (:export :route-auth))
(in-package :mita.auth.server.ningle)

(defmacro with-admin-db ((db connector) &body body)
  `(mita.admin:with-admin-db (,db ,connector)
     ,@body))

(defclass account-repository ()
  ((connector :initarg :connector)))

(defmethod mita.util.auth:account-identity
    ((account mita.admin.account:account))
  (mita.id:to-string (mita.admin.account:account-id account)))

(defmethod mita.util.auth:find-account ((repos account-repository)
                                        username
                                        password)
  (when (and username password)
    (with-admin-db (db (slot-value repos 'connector))
      (mita.admin.account:find-account-with-password-checked
       db username password))))

;;;

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

(defmacro with-safe-json-response (&body body)
  `(let ((resp ningle:*response*))
     (alexandria:appendf (lack.response:response-headers resp)
                         (list :content-type "application/json"))
     (handler-case
         (jsown:to-json
          (jsown:new-js
            ("success" t)
            ("value" (progn ,@body))))
       (error (e)
         (warn "Error: ~A" e)
         (setf (lack.response:response-status ningle:*response*)
               500)
         (jsown:to-json
          (jsown:new-js
            ("success" :f)))))))

(defun route-auth (app connector &key top-url)
  (setf (ningle:route app "/auth/login")
        (lambda (params)
          (declare (ignore params))
          (if (mita.util.auth:is-authenticated-p
               (make-instance
                'mita.util.auth.lack:lack-session-holder
                :env (lack.request:request-env ningle:*request*)))
              `(302 (:location ,top-url) nil)
              (login-page))))

  (setf (ningle:route app "/auth/api/logout" :method :post)
        (lambda (params)
          (declare (ignore params))
          (with-safe-json-response
            (let ((env (lack.request:request-env ningle:*request*)))
              (mita.util.auth:logout
               (make-instance 'mita.util.auth.lack:lack-session-holder
                              :env env)))
            t)))

  (setf (ningle:route app "/auth/api/login" :method :post)
        (lambda (params)
          (declare (ignore params))
          (with-safe-json-response
            (let ((env
                   (lack.request:request-env ningle:*request*))
                  (body
                   (lack.request:request-body-parameters ningle:*request*)))
              (if (mita.util.auth:login
                   (make-instance 'mita.util.auth.lack:lack-session-holder
                                  :env env)
                   (make-instance 'account-repository
                                  :connector connector)
                   (cdr (assoc "username" body :test #'string=))
                   (cdr (assoc "password" body :test #'string=)))
                  t
                  :false))))))
