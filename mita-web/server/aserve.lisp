(defpackage :mita.web.server.aserve
  (:use :cl)
  (:import-from :alexandria
                :when-let
                :when-let*)
  (:export :start))
(in-package :mita.web.server.aserve)

(defclass aserve-session-holder (mita.auth:session-holder)
  ((req :initarg :req)
   (session-store :initarg :session-store)))

(defmethod mita.auth:get-session ((holder aserve-session-holder))
  (with-slots (req session-store) holder
    (or (when-let ((sid (cdr (assoc "lack.session"
                                    (net.aserve:get-cookie-values req)
                                    :test #'string=))))
          (mita.auth.session:fetch-session session-store sid))
        (make-hash-table :test #'equal))))

(defmacro ensure-authenticated ((account req &key session-store connector)
                                &body body)
  `(when-let ((,account
               (mita.auth:is-authenticated-p
                (make-instance 'aserve-session-holder
                               :req ,req
                               :session-store ,session-store)
                ,connector)))
     (progn ,@body)))



(defstruct req account)

(defmethod mita.web.server:request-account ((req req))
  (req-account req))

(defun start (&key (port 5003)
                   content-root
                   thumbnail-root
                   (session-store mita.auth.server:*session-store*)
                   connector)
  (net.aserve:start :port port
                    :host "0.0.0.0")
  (net.aserve:publish-prefix
   :prefix "/images/"
   :function
   (lambda (req ent)
     (ensure-authenticated (account req
                            :connector connector
                            :session-store session-store)
       (mita.web.server:serve-image
        (make-instance 'mita.web.server:server
                       :connector connector
                       :thumbnail-root thumbnail-root
                       :content-root content-root)
        (make-req :account account)
        ;; (length "/images/")
        (subseq (puri:uri-path (net.aserve:request-uri req)) 8)
        :on-found
        (lambda (path)
          (net.aserve:with-http-response (req ent)
            (net.aserve:with-http-body (req ent)
              (with-open-file (in path
                                  :direction :input
                                  :element-type '(unsigned-byte 8))
                (ignore-errors
                 (alexandria:copy-stream
                  in net.html.generator:*html-stream*))))))
        :on-not-found
        (lambda ()
          nil))))))
