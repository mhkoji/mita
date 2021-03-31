(defpackage :mita.server.aserve
  (:use :cl)
  (:import-from :alexandria
                :when-let
                :when-let*)
  (:export :start))
(in-package :mita.server.aserve)

(defclass aserve-session-holder (mita.util.auth:session-holder)
  ((req :initarg :req)
   (session-store :initarg :session-store)))

(defmethod mita.util.auth:get-session ((holder aserve-session-holder))
  (with-slots (req session-store) holder
    (or (when-let ((sid (cdr (assoc "lack.session"
                                    (net.aserve:get-cookie-values req)
                                    :test #'string=))))
          (mita.util.auth.session:fetch-session session-store sid))
        (make-hash-table :test #'equal))))

(defmacro ensure-authenticated ((account-id req &key session-store)
                                &body body)
  `(when-let ((,account-id
               (mita.util.auth:is-authenticated-p
                (make-instance 'aserve-session-holder
                               :req ,req
                               :session-store ,session-store))))
     (progn ,@body)))



(defstruct req account-id)

(defmethod mita.server.app:request-account-id ((req req))
  (req-account-id req))

(defun start (&key (port 5003)
                   content-base
                   thumbnail-base
                   (session-store mita.auth.server:*session-store*)
                   connector)
  (net.aserve:start :port port
                    :host "0.0.0.0")
  (net.aserve:publish-prefix
   :prefix "/images/"
   :function
   (lambda (req ent)
     (ensure-authenticated (account-id req :session-store session-store)
       (mita.server.app:image-serve
        (make-instance 'mita.server.app:spec
                       :connector connector
                       :content-base content-base
                       :thumbnail-base thumbnail-base)
        (make-req :account-id account-id)
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
