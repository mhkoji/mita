(defpackage :mita.web.server.clack.mita
  (:use :cl)
  (:export :make-middleware)
  (:import-from :alexandria
                :when-let
                :when-let*
                :if-let))
(in-package :mita.web.server.clack.mita)

(defun html-response (body-string &key (status-code 200))
  `(,status-code (:content-type "text/html") (,body-string)))

(defun json-response (body-string &key (status-code 200))
  `(,status-code (:content-type "application/json") (,body-string)))


(defvar +html-500+
  (html-response (mita.web.server.html:internal-server-error)
                 :status-code 500))

(defvar +html-404+
  (html-response (mita.web.server.html:not-found)
                 :status-code 404))

(defmacro with-safe-json-response (&body body)
  `(handler-case
       (json-response (jsown:to-json
                        (jsown:new-js
                          ("success" t)
                          ("value" (progn ,@body)))))
     (error (e)
       (warn "Error: ~A" e)
       (json-response (jsown:to-json
                        (jsown:new-js
                          ("success" :f)))
                      :status-code 500))))


(defmacro with-safe-html-response (&body body)
  `(handler-case
       (html-response (or (progn ,@body) (assert nil)))
     (error ()
       +html-500+)))

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
           (mita.web.server.html:pages pages))))))

  (connect
   mapper "/pages/:page-id"
   (lambda (params req)
     (declare (ignore req))
     (with-safe-html-response
       (with-db (db connector)
         (or (when-let*
                 ((page-id
                   (mita.id:parse-short-or-nil
                    (cdr (getf params :page-id))))
                  (page
                   (mita.page:load-page-by-id db page-id)))
               (mita.web.server.html:page db page))
             +html-404+)))))

  (connect
   mapper "/api/pages/:page-id/text"
   (lambda (params req)
     (declare (ignore req))
     (with-json-api (db connector)
       (or (when-let*
               ((page-id
                 (mita.id:parse-short-or-nil
                  (cdr (getf params :page-id))))
                (page
                 (mita.page:load-page-by-id db page-id))
                (text
                 (cdr (assoc "text" (getf *env* :body-parameters)
                             :test #'string=))))
             (mita.page:update-page-text db page text))
           +html-404+)))
   :method :put)

  (connect
   mapper "/api/pages/_create"
   (lambda (params req)
     (declare (ignore params req))
     (with-json-api (db connector)
       (let ((page (mita.page:create-page db)))
         (jsown:new-js
           ("redirect"
            (mita.web.server.jsown:url-for page))))))
   :method :post))

(defun connect-dir (mapper connector thumbnail-root content-root)
  (setq content-root (namestring content-root))
  (setq thumbnail-root (namestring thumbnail-root))
  (connect
   mapper "/dir/*"
   (lambda (params req)
     (declare (ignore req))
     (or (when-let ((path (car (getf params :splat))))
           (let ((full-path (parse-namestring
                             (concatenate 'string content-root "/" path))))
             (when (cl-fad:file-exists-p full-path)
               (if (cl-fad:directory-pathname-p full-path)
                   (html-response
                    (mita.web.server.html:dir
                     (mita.dir:as-file content-root full-path)))
                   `(200 () ,full-path)))))
         +html-404+)))

  (connect
   mapper "/api/dir/add-albums"
   (lambda (params req)
     (declare (ignore params))
     (with-safe-json-response
       (with-db (db connector)
         (when-let ((path (q req "path")))
           (let ((full-path (parse-namestring
                             (concatenate 'string content-root "/" path))))
             (when (cl-fad:file-exists-p full-path)
               (let ((dirs (mita.dir:list-dirs content-root full-path)))
                 (mita.add-albums:run db dirs thumbnail-root))
               :t))))))
   :method :post))

(defun connect-image (mapper connector thumbnail-root content-root)
  (connect
   mapper "/images/:image-id"
   (lambda (params req)
     (declare (ignore req))
     (with-db (db connector)
       (or (when-let*
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
                                (mita.image:image-path image)))))
           +html-404+)))))

(defun connect-album (mapper connector)
  (connect
   mapper "/albums"
   (lambda (params req)
     (declare (ignore params))
     (with-safe-html-response
       (with-db (db connector)
         (mita.web.server.html:albums
          db
          (if-let ((offset (q req "offset")))
            (parse-integer offset)
            0)
          (if-let ((limit (q req "limit")))
            (parse-integer limit)
            50))))))

  (connect
   mapper "/albums/:album-id"
   (lambda (params req)
     (declare (ignore req))
     (with-safe-html-response
       (with-db (db connector)
         (or (when-let*
                 ((album-id
                   (mita.id:parse-short-or-nil
                    (getf params :album-id)))
                  (album
                   (mita.album:load-album-by-id db album-id)))
               (mita.web.server.html:album db album))
             +html-404+))))))

(defun connect-view (mapper connector)
  (connect
   mapper "/view/album/:album-id"
   (lambda (params req)
     (declare (ignore req))
     (with-safe-html-response
       (with-db (db connector)
         (or (when-let*
                 ((album-id
                   (mita.id:parse-short-or-nil
                    (getf params :album-id)))
                  (album
                   (mita.album:load-album-by-id db album-id)))
               (mita.web.server.html:view
                (mita.album:album-images db album)))
             +html-404+))))))

(defun connect-home (mapper)
  (connect
   mapper "/"
   (lambda (params req)
     (declare (ignore params req))
     (with-safe-html-response
       (mita.web.server.html:home)))))

(defun connect-tag (mapper connector)
  (connect
   mapper "/tags"
   (lambda (params req)
     (declare (ignore params req))
     (with-safe-html-response
       (with-db (db connector)
         (mita.web.server.html:tags db)))))

  (connect
   mapper "/api/tags/_create"
   (lambda (params req)
     (declare (ignore params))
     (with-json-api (db connector)
       (let ((name (q req "name")))
         (mita.tag:create-tag db name)
         (values))))
   :method :post)

  
  (connect
   mapper "/api/tags"
   (lambda (params req)
     (declare (ignore params req))
     (with-json-api (db connector)
       (mita.tag:load-tags db))))

  (connect
   mapper "/api/tags/:tag-id"
   (lambda (params req)
     (declare (ignore req))
     (with-json-api (db connector)
       (when-let*
           ((tag-id
             (mita.id:parse-short-or-nil
              (getf params :tag-id))))
         (mita.tag:delete-tag db tag-id))))
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
         (mita.tag:update-tag-name db tag name))))
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
         (mapcar #'mita.web.server.jsown:as-content
                 (mita.tag:tag-contents db tag))))))


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
         (mita.tag:content-tags db album)))))

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
            db album (remove nil nullable-tag-id-list))))))
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
