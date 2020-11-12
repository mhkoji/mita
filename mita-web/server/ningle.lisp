(defpackage :mita.web.server.ningle
  (:use :cl)
  (:export :route-tag
           :route-page
           :route-view
           :route-home
           :route-image
           :route-album
           :route-dir
           :route-auth)
  (:import-from :alexandria
                :when-let
                :when-let*
                :if-let))
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
            ("value" (progn ,@body))))
       (error (e)
         (warn "Error: ~A" e)
         (setf (lack.response:response-status ningle:*response*)
               500)
         (jsown:to-json
          (jsown:new-js
            ("success" :f)))))))


(defmacro with-safe-html-response (&body body)
  `(handler-case (or (progn ,@body) (assert nil))
     (error ()
       +response-500+)))

(defmacro with-gateway ((gw connector &key (request 'ningle:*request*))
                        &body body)
  (let ((g (gensym)))
    `(let ((,g (getf (lack.request:request-env ,request)
                     :mita.account)))
       (mita.postgres:with-gateway (,gw ,g ,connector)
         ,@body))))

(defmacro with-json-api ((gw connector &key (request 'ningle:*request*))
                         &body body)
  `(with-safe-json-response
     (with-gateway (,gw ,connector :request ,request)
       ,@body)))

(defun q (params name)
  (cdr (assoc name params :test #'string=)))

(defun route-page (app connector)
  (setf (ningle:route app "/pages")
        (lambda (params)
          (declare (ignore params))
          (with-safe-html-response
            (with-gateway (gw connector)
              (let ((pages (mita.page:load-pages gw)))
                (mita.web.server.html:pages pages))))))

  (setf (ningle:route app "/pages/:page-id")
        (lambda (params)
          (with-safe-html-response
            (with-gateway (gw connector)
              (or (when-let*
                      ((page-id
                        (mita.id:parse-short-or-nil
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
                      (mita.id:parse-short-or-nil
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
                 (mita.web.server.jsown:url-for page))))))))

(defun route-dir (app connector thumbnail-root content-root)
  (setq content-root (namestring content-root))
  (setq thumbnail-root (namestring thumbnail-root))
  (setf (ningle:route app "/dir/*")
        (lambda (params)
          (or (when-let ((path (cadr (assoc :splat params))))
                (let ((full-path (parse-namestring
                                  (concatenate 'string
                                   content-root "/" path))))
                  (when (cl-fad:file-exists-p full-path)
                    (if (cl-fad:directory-pathname-p full-path)
                        (mita.web.server.html:dir
                         (mita.dir:as-file content-root full-path))
                        full-path))))
              +response-404+)))

  (setf (ningle:route app "/api/dir/add-albums" :method :post)
        (lambda (params)
          (with-safe-json-response
            (with-gateway (gw connector)
              (when-let ((path (q params "path")))
                (let ((full-path (parse-namestring
                                  (concatenate 'string
                                   content-root "/" path))))
                  (when (cl-fad:file-exists-p full-path)
                    (let ((dirs (mita.dir:list-dirs content-root full-path)))
                      (mita.add-albums:run gw dirs thumbnail-root))
                    :t))))))))

(defun route-image (app connector thumbnail-root content-root)
  (setf (ningle:route app "/images/:image-id")
        (lambda (params)
          (with-safe-html-response
            (with-gateway (gw connector)
              (or (when-let*
                      ((image-id
                        (mita.id:parse-short-or-nil
                           (cdr (assoc :image-id params))))
                       (image
                        (mita.image:load-image gw image-id))
                       (root
                        (cadr (assoc
                               (mita.image:image-source image)
                               (list (list mita.image:+source-content+
                                           content-root)
                                     (list mita.image:+source-thumbnail+
                                           thumbnail-root))))))
                    (parse-namestring
                     (format nil "~A/~A"
                             root
                             (mita.image:image-path image))))
                  +response-404+))))))

(defun route-album (app connector)
  (setf (ningle:route app "/albums")
        (lambda (params)
          (with-safe-html-response
            (with-gateway (gw connector)
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
            (with-gateway (gw connector)
              (or (when-let*
                      ((album-id
                        (mita.id:parse-short-or-nil
                         (cdr (assoc :album-id params))))
                       (album
                        (mita.album:load-album-by-id gw album-id)))
                    (mita.web.server.html:album gw album))
                  +response-404+))))))

(defun route-view (app connector)
  (setf (ningle:route app "/view/album/:album-id")
        (lambda (params)
          (with-safe-html-response
            (with-gateway (gw connector)
              (or (when-let*
                      ((album-id
                        (mita.id:parse-short-or-nil
                         (cdr (assoc :album-id params))))
                       (album
                        (mita.album:load-album-by-id gw album-id)))
                    (mita.web.server.html:view
                     (mita.album:album-images gw album)))
                  +response-404+))))))

(defun route-home (app)
  (setf (ningle:route app "/")
        (lambda (params)
          (declare (ignore params))
          (with-safe-html-response
            (mita.web.server.html:home)))))

(defun route-tag (app connector)
  (setf (ningle:route app "/tags")
        (lambda (params)
          (declare (ignore params))
          (with-safe-html-response
            (with-gateway (gw connector)
              (mita.web.server.html:tags gw)))))

  (setf (ningle:route app "/api/tags/_create" :method :post)
        (lambda (params)
          (with-json-api (gw connector)
            (let ((name (q params "name")))
              (mita.tag:create-tag gw name)
              (values)))))


    (setf (ningle:route app "/api/tags")
          (lambda (params)
            (declare (ignore params))
            (with-json-api (gw connector)
              (mita.tag:load-tags gw))))

    (setf (ningle:route app "/api/tags/:tag-id" :method :delete)
          (lambda (params)
            (with-json-api (gw connector)
              (when-let*
                  ((tag-id
                    (mita.id:parse-short-or-nil
                     (cdr (assoc :tag-id params)))))
                (mita.tag:delete-tag gw tag-id)))))

    (setf (ningle:route app "/api/tags/:tag-id" :method :put)
        (lambda (params)
          (with-json-api (gw connector)
            (when-let*
                ((name
                  (q params "name"))
                 (tag-id
                  (mita.id:parse-short-or-nil
                   (cdr (assoc :tag-id params))))
                 (tag
                  (mita.tag:load-tag-by-id gw tag-id)))
              (mita.tag:update-tag-name gw tag name)))))

    (setf (ningle:route app "/api/tags/:tag-id/contents" :method :get)
          (lambda (params)
            (with-json-api (gw connector)
              (when-let*
                  ((tag-id
                    (mita.id:parse-short-or-nil
                     (cdr (assoc :tag-id params))))
                   (tag
                    (mita.tag:load-tag-by-id gw tag-id)))
                (mapcar #'mita.web.server.jsown:as-content
                        (mita.tag:tag-contents gw tag))))))


    (setf (ningle:route app "/api/albumTags/:album-id")
          (lambda (params)
            (with-json-api (gw connector)
              (when-let*
                  ((album-id
                    (mita.id:parse-short-or-nil
                     (cdr (assoc :album-id params))))
                   (album
                    (mita.album:load-album-by-id gw album-id)))
                (mita.tag:content-tags gw album)))))


    (setf (ningle:route app "/api/albumTags/:album-id" :method :put)
          (lambda (params)
            (with-json-api (gw connector)
              (when-let*
                  ((album-id
                    (mita.id:parse-short-or-nil
                     (cdr (assoc :album-id params))))
                   (album
                    (mita.album:load-album-by-id gw album-id)))
                (let ((nullable-tag-id-list
                       (mapcar
                        #'mita.id:parse-short-or-nil
                        (cdr (assoc "tag-id-list"
                                    (lack.request:request-body-parameters
                                     ningle:*request*)
                                    :test #'string=)))))
                  (mita.tag:update-content-tags
                   gw album (remove nil nullable-tag-id-list))))))))
