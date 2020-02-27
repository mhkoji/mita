(defpackage :mita.web.server.ningle
  (:use :cl)
  (:import-from :alexandria
                :when-let*)
  (:export :route-page
           :route-image
           :route-album))
(in-package :mita.web.server.ningle)

(defvar +response-500+
  `(500 (:content-type "text/html")
        (,(mita.web.server.html:internal-server-error))))

(defvar +response-404+
  `(404 (:content-type "text/html")
        (,(mita.web.server.html:not-found))))

(defmacro with-safe-json-response (&body body)
  `(let ((resp ningle:*response*))
     (alexandria:appendf (lack.response:response-headers resp)
                         (list :content-type "application/json"))
     (handler-case
         (jsown:to-json
          (jsown:new-js
            ("success" t)
            ("value" (or (progn ,@body) (assert nil)))))
       (error ()
         (setf (lack.response:response-status ningle:*response*)
               500)
         (jsown:to-json
          (jsown:new-js
            ("success" :f)))))))

(defmacro with-safe-html-response (&body body)
  `(handler-case (or (progn ,@body) (assert nil))
     (error ()
       +response-500+)))

(defmacro with-json-api ((gw connector) &body body)
  `(with-safe-json-response
     (mita:with-gateway (,gw ,connector)
       ,@body)))

(defun q (params name)
  (cdr (assoc name params :test #'string=)))

(defun route-page (app connector)
  (setf (ningle:route app "/pages")
        (lambda (params)
          (declare (ignore params))
          (with-safe-html-response
            (mita:with-gateway (gw connector)
              (let ((pages (mita.page:load-pages gw)))
                (mita.web.server.html:pages pages))))))

  (setf (ningle:route app "/pages/:page-id")
        (lambda (params)
          (with-safe-html-response
            (mita:with-gateway (gw connector)
              (or (when-let*
                      ((page-id
                        (mita.id:parse-or-nil
                         (cdr (assoc :page-id params))))
                       (page
                        (mita.page:load-page-by-id gw page-id)))
                    (mita.web.server.html:page gw page))
                  +response-404+)))))

  (setf (ningle:route app "/api/pages/:page-id/text" :method :put)
        (lambda (params)
          (with-json-api (gw connector)
            (or (when-let*
                    ((page-id
                      (mita.id:parse-or-nil
                       (cdr (assoc :page-id params))))
                     (page
                      (mita.page:load-page-by-id gw page-id))
                     (text
                      (cdr (assoc "text"
                                  (lack.request:request-body-parameters
                                   ningle:*request*)
                                  :test #'string=))))
                  (mita.page:update-page-text gw page text))
                +response-404+))))

  (setf (ningle:route app "/api/pages/_create" :method :post)
        (lambda (params)
          (declare (ignore params))
          (with-json-api (gw connector)
            (let ((page (mita.page:create-page gw)))
              (jsown:new-js
                ("redirect"
                 (format nil "/pages/~A"
                         (mita.id:to-string (mita.page:page-id page))))))))))


(defun route-image (app connector)
  (setf (ningle:route app "/images/:image-id")
        (lambda (params)
          (with-safe-html-response
            (mita:with-gateway (gw connector)
              (or (when-let*
                      ((image-id
                        (mita.id:parse-or-nil
                           (cdr (assoc :image-id params))))
                       (image
                        (mita.image:load-image gw image-id)))
                    (cl-fad:pathname-as-file
                     (mita.image:image-path image)))
                  +response-404+))))))

(defun route-album (app connector)
  (setf (ningle:route app "/albums")
        (lambda (params)
          (with-safe-html-response
            (mita:with-gateway (gw connector)
              (let ((offset (or (q params "offset") 0))
                    (limit  (or (q params "limit") 50)))
                (mita.web.server.html:albums
                 (mita.album:load-albums gw offset limit))))))))
