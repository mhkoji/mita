(defpackage :mita.web.server.ningle
  (:use :cl)
  (:export :route-tag
           :route-page
           :route-view
           :route-image
           :route-album
           :route-auth)
  (:import-from :alexandria
                :when-let
                :when-let*
                :if-let))
(in-package :mita.web.server.ningle)

(defgeneric url-for (content))

(defmethod url-for ((c mita.album:album))
  (format nil "/albums/~A" (mita.id:to-string (mita.album:album-id c))))

(defmethod url-for ((c mita.page:page))
  (format nil "/pages/~A" (mita.id:to-string (mita.page:page-id c))))


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
            ("value" (progn ,@body))))
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
              (mita.web.server.html:albums
               gw
               (if-let ((offset (q params "offset")))
                 (parse-integer offset)
                 0)
               (if-let ((limit (q params "limit")))
                 (parse-integer limit)
                 50))))))

  (setf (ningle:route app "/albums/:album-id")
        (lambda (params)
          (with-safe-html-response
            (mita:with-gateway (gw connector)
              (or (when-let*
                      ((album-id
                        (mita.id:parse-or-nil
                         (cdr (assoc :album-id params))))
                       (album
                        (mita.album:load-album-by-id gw album-id)))
                    (mita.web.server.html:album gw album))
                  +response-404+))))))

(defun route-view (app connector)
  (setf (ningle:route app "/view/album/:album-id")
        (lambda (params)
          (with-safe-html-response
            (mita:with-gateway (gw connector)
              (or (when-let*
                      ((album-id
                        (mita.id:parse-or-nil
                         (cdr (assoc :album-id params))))
                       (album
                        (mita.album:load-album-by-id gw album-id)))
                    (mita.web.server.html:view
                     (mita.album:album-images gw album)))
                  +response-404+))))))


(defun route-tag (app connector)
  (setf (ningle:route app "/tags")
        (lambda (params)
          (declare (ignore params))
          (with-safe-html-response
            (mita:with-gateway (gw connector)
              (mita.web.server.html:tags gw)))))

  (setf (ningle:route app "/api/tags/_create" :method :post)
        (lambda (params)
          (with-json-api (gw connector)
            (let ((name (q params "name")))
              (mita.tag:create-tag gw name)
              (values)))))

  (labels ((tag->jsown (tag)
             (jsown:new-js
               ("id" (mita.id:to-string (mita.tag:tag-id tag)))
               ("name" (mita.tag:tag-name tag)))))

    (setf (ningle:route app "/api/tags")
          (lambda (params)
            (declare (ignore params))
            (with-json-api (gw connector)
              (let ((tags (mita.tag:load-tags gw)))
                (mapcar #'tag->jsown tags)))))

    (setf (ningle:route app "/api/tags/:tag-id" :method :delete)
          (lambda (params)
            (with-json-api (gw connector)
              (when-let*
                  ((tag-id
                    (mita.id:parse-or-nil
                     (cdr (assoc :tag-id params)))))
                (mita.tag:delete-tag gw tag-id)))))

    (setf (ningle:route app "/api/tags/:tag-id" :method :put)
        (lambda (params)
          (with-json-api (gw connector)
            (when-let*
                ((name
                  (q params "name"))
                 (tag-id
                  (mita.id:parse-or-nil
                   (cdr (assoc :tag-id params))))
                 (tag
                  (mita.tag:load-tag-by-id gw tag-id)))
              (mita.tag:update-tag-name gw tag name)))))

    (setf (ningle:route app "/api/tags/:tag-id/contents" :method :get)
          (lambda (params)
            (with-json-api (gw connector)
              (when-let*
                  ((tag-id
                    (mita.id:parse-or-nil
                     (cdr (assoc :tag-id params))))
                   (tag
                    (mita.tag:load-tag-by-id gw tag-id)))
                (mapcar (lambda (c)
                          (jsown:new-js
                            ("id"
                             (mita.id:to-string (mita.tag:content-id c)))
                            ("url"
                             (url-for c))
                            ("type"
                             (symbol-name (mita.tag:content-type c)))
                            ("name"
                             (or (mita.tag:content-name c) :null))
                            ("thumbnail"
                             (if-let ((image (mita.tag:content-thumbnail c)))
                               (jsown:new-js
                                 ("url"
                                  (format nil "/images/~A"
                                   (mita.id:to-string
                                    (mita.image:image-id image)))))
                               :null))))
                        (mita.tag:tag-contents gw tag))))))


    (setf (ningle:route app "/api/albumTags/:album-id")
          (lambda (params)
            (with-json-api (gw connector)
              (when-let*
                  ((album-id
                    (mita.id:parse-or-nil
                     (cdr (assoc :album-id params))))
                   (album
                    (mita.album:load-album-by-id gw album-id)))
                (let ((tags (mita.tag:content-tags gw album)))
                  (mapcar #'tag->jsown tags))))))

    (setf (ningle:route app "/api/albumTags/:album-id" :method :put)
          (lambda (params)
            (with-json-api (gw connector)
              (when-let*
                  ((album-id
                    (mita.id:parse-or-nil
                     (cdr (assoc :album-id params))))
                   (album
                    (mita.album:load-album-by-id gw album-id)))
                (let ((nullable-tag-id-list
                       (mapcar
                        #'mita.id:parse-or-nil
                        (cdr (assoc "tag-id-list"
                                    (lack.request:request-body-parameters
                                     ningle:*request*)
                                    :test #'string=)))))
                  (mita.tag:update-content-tags
                   gw album (remove nil nullable-tag-id-list)))))))))


(defun route-auth (app connector)
  (setf (ningle:route app "/auth/login")
        (lambda (params)
          (declare (ignore params))
          (if (mita.web.auth:is-authenticated-p
               (getf (lack.request:request-env ningle:*request*)
                     :lack.session))
              `(300 (:location "/albums") nil)
              (with-safe-html-response
                (mita.web.server.html:login)))))

  (setf (ningle:route app "/api/authenticate" :method :post)
        (lambda (params)
          (declare (ignore params))
          (with-json-api (gw connector)
            (if (mita.web.auth:authenticate
                 (getf (lack.request:request-env ningle:*request*)
                       :lack.session)
                 (lambda ()
                   (let ((params (lack.request:request-body-parameters
                                  ningle:*request*)))
                     (when-let ((username (cdr (assoc "username" params
                                                      :test #'string=)))
                                (password (cdr (assoc "password" params
                                                      :test #'string=))))
                       (mita.account:find-account gw username password)))))
                 t
                :false)))))
