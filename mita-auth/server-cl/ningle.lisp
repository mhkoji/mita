(defpackage :mita.auth.ningle
  (:use :cl)
  (:export :route-auth))
(in-package :mita.auth.ningle)

(defun route-auth (app &key top-url login-page-fn is-allowed-fn)
  (setf (ningle:route app "/auth/login")
        (lambda (params)
          (declare (ignore params))
          (if (mita.auth:is-authenticated-p
               (make-instance
                'mita.auth:lack-session-holder
                :env (lack.request:request-env ningle:*request*)))
              `(300 (:location ,top-url) nil)
              (funcall login-page-fn))))

  (setf (ningle:route app "/auth/api/authenticate" :method :post)
        (lambda (params)
          (declare (ignore params))
          (mita.web.server.ningle::with-safe-json-response
            (if (mita.auth:authenticate
                 (make-instance
                  'mita.auth:lack-session-holder
                  :env (lack.request:request-env ningle:*request*))
                 is-allowed-fn)
                t
                :false)))))
