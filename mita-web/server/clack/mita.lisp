(defpackage :mita.web.server.clack.mita
  (:use :cl)
  (:export :make-middleware)
  (:import-from :alexandria
                :when-let
                :when-let*
                :if-let))
(in-package :mita.web.server.clack.mita)

(defun html-response (body-string &key (status-code 200))
  `(,status-code (:content-type "text/html")
                 (,body-string)))

(defun json-response (value &key (status-code 200) (success t))
  `(,status-code (:content-type "application/json")
                 (,(jsown:to-json
                    (jsown:new-js
                      ("success" (or success :f))
                      ("value" value))))))

(defmacro with-safe-json-response (&body body)
  `(handler-case
       (or (progn ,@body)
           (json-response (jsown:new-js)
                          :success nil
                          :status-code 404))
     (error (e)
       (warn "Error: ~A" e)
       (json-response (jsown:new-js)
                      :success nil
                      :status-code 500))))

(defmacro with-safe-html-response (&body body)
  `(handler-case
       (or (progn ,@body)
           (html-response (mita.web.server.html:not-found)
                          :status-code 404))
     (error (e)
       (warn "Error: ~A" e)
       (html-response (mita.web.server.html:internal-server-error)
                      :status-code 500))))


(defvar *request*)

(defmacro with-db ((db connector) &body body)
  `(mita.postgres:with-db
       (,db (getf (lack.request:request-env *request*) :mita.account)
            ,connector)
     ,@body))

(defmacro with-json-api ((db connector) &body body)
  `(with-safe-json-response
     (with-db (,db ,connector)
       ,@body)))

(defun q (req name)
  (let ((params (lack.request:request-parameters req)))
    (cdr (assoc name params :test #'string=))))

(defmacro connect (mapper url fn &rest keys)
  `(myway:connect ,mapper ,url
                  (lambda (params)
                    (lambda (req)
                      (,fn params req)))
                  ,@keys))

(defun mapper->middleware (mapper)
  (lambda (app)
    (lambda (env)
      (or (let ((request (lack.request:make-request env)))
            (multiple-value-bind (handler foundp)
                (myway:dispatch
                 mapper (lack.request:request-path-info request)
                 :method (lack.request:request-method request))
              (when foundp
                (let ((*request* request))
                  (funcall handler request)))))
          (funcall app env)))))

(defun connect-page (mapper connector)
  (connect
   mapper "/pages"
   (lambda (params req)
     (declare (ignore params req))
     (with-safe-html-response
       (with-db (db connector)
         (let ((pages (mita.page:load-pages db)))
           (html-response
            (mita.web.server.html:pages pages)))))))

  (connect
   mapper "/pages/:page-id"
   (lambda (params req)
     (declare (ignore req))
     (with-safe-html-response
       (with-db (db connector)
         (when-let*
             ((page-id
               (mita.id:parse-short-or-nil
                (getf params :page-id)))
              (page
               (mita.page:load-page-by-id db page-id)))
           (html-response
            (mita.web.server.html:page db page)))))))

  (connect
   mapper "/api/pages/:page-id/text"
   (lambda (params req)
     (with-json-api (db connector)
       (when-let*
           ((page-id
             (mita.id:parse-short-or-nil
              (getf params :page-id)))
            (page
             (mita.page:load-page-by-id db page-id))
            (text
             (cdr (assoc "text"
                         (lack.request:request-body-parameters req)
                         :test #'string=))))
         (json-response
          (mita.page:update-page-text db page text)))))
   :method :put)

  (connect
   mapper "/api/pages/_create"
   (lambda (params req)
     (declare (ignore params req))
     (with-json-api (db connector)
       (let ((page (mita.page:create-page db)))
         (json-response
          (jsown:new-js
            ("redirect"
             (mita.web.server.jsown:url-for page)))))))
   :method :post))

(defun connect-dir (mapper connector thumbnail-root content-root)
  (setq content-root (namestring content-root))
  (setq thumbnail-root (namestring thumbnail-root))
  (connect
   mapper "/dir/*"
   (lambda (params req)
     (declare (ignore req))
     (with-safe-html-response
       (when-let ((path (car (getf params :splat))))
         (let ((full-path
                 (parse-namestring
                  (concatenate 'string content-root "/" path))))
           (when (cl-fad:file-exists-p full-path)
             (if (cl-fad:directory-pathname-p full-path)
                 (html-response
                  (mita.web.server.html:dir
                   (mita.dir:as-file content-root full-path)))
                 `(200 () ,full-path))))))))

  (connect
   mapper "/api/dir/add-albums"
   (lambda (params req)
     (declare (ignore params))
     (with-safe-json-response
       (with-db (db connector)
         (when-let ((path (q req "path")))
           (let ((full-path
                   (parse-namestring
                    (concatenate 'string content-root "/" path))))
             (when (cl-fad:file-exists-p full-path)
               (let ((dirs (mita.dir:list-dirs content-root full-path)))
                 (mita.add-albums:run db dirs thumbnail-root))
               (json-response (jsown:new-js))))))))
   :method :post))

(defun connect-image (mapper connector thumbnail-root content-root)
  (connect
   mapper "/images/:image-id"
   (lambda (params req)
     (declare (ignore req))
     (with-db (db connector)
       (with-safe-html-response
         (when-let*
             ((image-id
               (mita.id:parse-short-or-nil (getf params :image-id)))
              (image
               (mita.image:load-image db image-id))
              (root
               (cadr (assoc
                      (mita.image:image-source image)
                      (list (list mita.image:+source-content+
                                  content-root)
                            (list mita.image:+source-thumbnail+
                                  thumbnail-root))))))
           `(200 () ,(parse-namestring
                      (format nil "~A/~A"
                              root
                              (mita.image:image-path image))))))))))

(defun connect-album (mapper connector)
  (connect
   mapper "/albums"
   (lambda (params req)
     (declare (ignore params))
     (with-safe-html-response
       (with-db (db connector)
         (html-response
          (mita.web.server.html:albums
           db
           (if-let ((offset (q req "offset")))
             (parse-integer offset)
             0)
           (if-let ((limit (q req "limit")))
             (parse-integer limit)
             50)))))))

  (connect
   mapper "/albums/:album-id"
   (lambda (params req)
     (declare (ignore req))
     (with-safe-html-response
       (with-db (db connector)
         (when-let*
             ((album-id
               (mita.id:parse-short-or-nil
                (getf params :album-id)))
              (album
               (mita.album:load-album-by-id db album-id)))
           (html-response
            (mita.web.server.html:album db album))))))))

(defun connect-view (mapper connector)
  (connect
   mapper "/view/album/:album-id"
   (lambda (params req)
     (declare (ignore req))
     (with-safe-html-response
       (with-db (db connector)
         (when-let*
             ((album-id
               (mita.id:parse-short-or-nil
                (getf params :album-id)))
              (album
               (mita.album:load-album-by-id db album-id)))
           (html-response
            (mita.web.server.html:view
             (mita.album:album-images db album)))))))))

(defun connect-home (mapper)
  (connect
   mapper "/"
   (lambda (params req)
     (declare (ignore params req))
     (with-safe-html-response
       (html-response (mita.web.server.html:home))))))

(defun connect-tag (mapper connector)
  (connect
   mapper "/tags"
   (lambda (params req)
     (declare (ignore params req))
     (with-safe-html-response
       (with-db (db connector)
         (html-response (mita.web.server.html:tags db))))))

  (connect
   mapper "/api/tags/_create"
   (lambda (params req)
     (declare (ignore params))
     (with-json-api (db connector)
       (let ((name (q req "name")))
         (mita.tag:create-tag db name)
         (json-response (jsown:new-js)))))
   :method :post)

  
  (connect
   mapper "/api/tags"
   (lambda (params req)
     (declare (ignore params req))
     (with-json-api (db connector)
       (json-response (mita.tag:load-tags db)))))

  (connect
   mapper "/api/tags/:tag-id"
   (lambda (params req)
     (declare (ignore req))
     (with-json-api (db connector)
       (when-let*
           ((tag-id
             (mita.id:parse-short-or-nil
              (getf params :tag-id))))
         (mita.tag:delete-tag db tag-id)
         (json-response (jsown:new-js)))))
   :method :delete)

  (connect
   mapper "/api/tags/:tag-id"
   (lambda (params req)
     (with-json-api (db connector)
       (when-let*
           ((name
             (q req "name"))
            (tag-id
             (mita.id:parse-short-or-nil
              (getf params :tag-id)))
            (tag
             (mita.tag:load-tag-by-id db tag-id)))
         (mita.tag:update-tag-name db tag name)
         (json-response (jsown:new-js)))))
   :method :put)

  (connect
   mapper "/api/tags/:tag-id/contents"
   (lambda (params req)
     (declare (ignore req))
     (with-json-api (db connector)
       (when-let*
           ((tag-id
             (mita.id:parse-short-or-nil
              (getf params :tag-id)))
            (tag
             (mita.tag:load-tag-by-id db tag-id)))
         (json-response
          (mapcar #'mita.web.server.jsown:as-content
                  (mita.tag:tag-contents db tag)))))))


  (connect
   mapper "/api/albumTags/:album-id"
   (lambda (params req)
     (declare (ignore req))
     (with-json-api (db connector)
       (when-let*
           ((album-id
             (mita.id:parse-short-or-nil
              (getf params :album-id)))
            (album
             (mita.album:load-album-by-id db album-id)))
         (json-response
          (mita.tag:content-tags db album))))))

  (connect
   mapper "/api/albumTags/:album-id"
   (lambda (params req)
     (with-json-api (db connector)
       (when-let*
           ((album-id
             (mita.id:parse-short-or-nil
              (getf params :album-id)))
            (album
             (mita.album:load-album-by-id db album-id)))
         (let ((nullable-tag-id-list
                 (mapcar
                  #'mita.id:parse-short-or-nil
                  (cdr (assoc "tag-id-list"
                              (lack.request:request-body-parameters req)
                              :test #'string=)))))
           (mita.tag:update-content-tags
            db album (remove nil nullable-tag-id-list))
           (json-response (jsown:new-js))))))
   :method :put))
  
(defun make-middleware (connector &key thumbnail-root
                                       content-root
                                       serve-image-p)
  (let ((mapper (myway:make-mapper)))
    (connect-album mapper connector)
    (connect-view mapper connector)
    (connect-page mapper connector)
    (connect-tag mapper connector)
    (connect-home mapper)
    (connect-dir mapper connector thumbnail-root content-root)
    (when serve-image-p
      (connect-image mapper connector thumbnail-root content-root))
    (mapper->middleware mapper)))
