(defpackage :mita.web.server.clack.log
  (:use :cl)
  (:export :make-middleware))
(in-package :mita.web.server.clack.log)

(defun make-middleware (&key (stream *standard-output*))
  (lambda (app)
    (lambda (env)
      (let ((id #+sbcl
                (sb-thread:thread-name sb-thread:*current-thread*)
                #-sbcl
                (uuid:make-v4-uuid)))
        (format stream "~A ~A ~A ~A before~%"
                (local-time:to-rfc3339-timestring (local-time:now))
                id
                (getf env :request-method)
                (getf env :request-uri))
        (force-output stream)
        (prog1 (funcall app env)
          (format stream "~A ~A after~%"
                (local-time:to-rfc3339-timestring (local-time:now))
                id)
          (force-output))))))
