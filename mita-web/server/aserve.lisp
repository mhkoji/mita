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

(defun start (&key (port 5003)
                   content-root
                   thumbnail-root
                   (session-store mita.auth.server:*session-store*)
                   connector)
  (net.aserve:start :port port :host "0.0.0.0")
  (net.aserve:publish-prefix
   :prefix "/images/"
   :function
   (lambda (req ent)
     (when-let ((account
                 (mita.auth:is-authenticated-p
                  (make-instance 'aserve-session-holder
                                 :req req
                                 :session-store session-store)
                  connector)))
       (mita.postgres:with-gateway (gw account connector)
          (when-let* ((image-id
                       (mita.id:parse-short-or-nil
                        (subseq (puri:uri-path (net.aserve:request-uri req))
                                8))) ;; (length "/images/")
                      (image
                       (mita.image:load-image gw image-id))
                      (root
                       (cadr (assoc
                              (mita.image:image-source image)
                              (list (list mita.image:+source-content+
                                          content-root)
                                    (list mita.image:+source-thumbnail+
                                          thumbnail-root))))))
            (let ((full-path (parse-namestring
                              (format nil "~A/~A"
                                      root
                                      (mita.image:image-path image)))))
              (net.aserve:with-http-response (req ent)
                (net.aserve:with-http-body (req ent)
                  (with-open-file (in full-path
                                      :direction :input
                                      :element-type '(unsigned-byte 8))
                    (ignore-errors
                      (alexandria:copy-stream
                       in net.html.generator:*html-stream*))))))))))))
