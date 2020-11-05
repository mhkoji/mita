(defpackage :mita.auth.ningle
  (:use :cl)
  (:export :route-auth))
(in-package :mita.auth.ningle)

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
       (error ()
         (setf (lack.response:response-status ningle:*response*)
               500)
         (jsown:to-json
          (jsown:new-js
            ("success" :f)))))))

(defun route-auth (app connector &key top-url)
  (setf (ningle:route app "/auth/login")
        (lambda (params)
          (declare (ignore params))
          (if (mita.auth:is-authenticated-p
               (make-instance
                'mita.auth:lack-session-holder
                :env (lack.request:request-env ningle:*request*))
               connector)
              `(302 (:location ,top-url) nil)
              (login-page))))

  (setf (ningle:route app "/auth/api/authenticate" :method :post)
        (lambda (params)
          (declare (ignore params))
          (with-safe-json-response
            (let ((env
                   (lack.request:request-env ningle:*request*))
                  (body
                   (lack.request:request-body-parameters ningle:*request*)))
              (if (mita.auth:authenticate
                   (make-instance 'mita.auth:lack-session-holder
                                  :env env)
                   connector
                   (cdr (assoc "username" body :test #'string=))
                   (cdr (assoc "password" body :test #'string=)))
                  t
                  :false))))))
