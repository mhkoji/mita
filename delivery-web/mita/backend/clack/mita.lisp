(defpackage :mita.web.clack.mita
  (:use :cl :mita.web.clack.util)
  (:export :make-middleware)
  (:import-from :mita.web.app
                :make-db
                :make-db-from-spec)
  (:import-from :alexandria
                :when-let
                :when-let*
                :if-let))
(in-package :mita.web.clack.mita)

(defmethod mita.web.app:request-account-id ((req lack.request:request))
  (getf (lack.request:request-env req) :mita.util.auth.identity))

;;;

(defun connect-dir (mapper spec)
  (connect-all mapper
   (("/dir/*"
     (lambda (params req)
       (mita.web.app:dir-serve
        spec req (car (getf params :splat))
        :on-found
        (lambda (file)
          (if (mita.fs:folder-p file)
              (html-response (mita.web.html:dir file))
              `(200 () ,(parse-namestring
                         (mita.fs.dir:file-full-path file)))))
        :on-not-found (lambda ()))))
    (("/api/dir/add-albums" :method :post)
     (lambda (params req)
       (declare (ignore params))
       (mita.web.app:dir-add-albums spec req (q req "path"))
       (json-response (jsown:new-js)))))))

(defun connect-image (mapper spec)
  (connect-all mapper
   (("/images/:image-id"
     (lambda (params req)
       (mita.web.app:image-serve
        spec req (getf params :image-id)
        :on-found (lambda (path) `(200 () ,path))
        :on-not-found (lambda () nil)))))))

(defun connect-album (mapper spec)
  (connect-all mapper
   (("/albums"
     (lambda (params req)
       (declare (ignore params))
       (let ((offset (ensure-integer (q req "offset") 0))
             (limit (ensure-integer (q req "limit") 50)))
         (multiple-value-bind (albums full-loaded-p)
             (mita.db:with-connection (conn (make-db-from-spec req spec))
               (mita.album:load-albums conn offset limit))
           (let ((format-str "/albums?offset=~A&limit=~A"))
             (html-response
              (mita.web.html:albums
               (if full-loaded-p (butlast albums) albums)
               (when (< 0 offset)
                 (format nil format-str (max (- offset limit) 0) limit))
               (when full-loaded-p
                 (format nil format-str (+ offset limit) limit)))))))))
    (("/albums" :method :post)
     (lambda (params req)
       (declare (ignore params))
       (let ((files
              (mapcar (lambda (item)
                        (destructuring-bind
                            (name stream path content-type) item
                          (declare (ignore name content-type))
                          (list path stream)))
                      (lack.request:request-body-parameters req))))
         (mita.web.app:album-upload spec req files))
       `(200 (:content-type "text/plain") ("OK"))))
    ("/albums/:album-id"
     (lambda (params req)
       (mita.db:with-connection (conn (make-db-from-spec req spec))
         (let ((album-id (ensure-uuid-short (getf params :album-id))))
           (if-let ((album (mita.album:load-album-by-id conn album-id)))
             (html-response (mita.web.html:album conn album))
             (html-response (mita.web.html:not-found)
                            :status-code 404)))))))))

(defun connect-view (mapper locator)
  (connect-all mapper
   (("/view/album/:album-id"
     (lambda (params req)
       (mita.db:with-connection (conn (make-db req locator))
         (let ((album-id (ensure-uuid-short (getf params :album-id))))
           (if-let ((album (mita.album:load-album-by-id conn album-id)))
             (html-response (mita.web.html:view
                             (mita.album:album-images conn album)))
             (html-response (mita.web.html:not-found)
                            :status-code 404)))))))))
  
(defun connect-home (mapper)
  (connect-all mapper
   (("/"
     (lambda (params req)
       (declare (ignore params req))
       (html-response (mita.web.html:home)))))))

(defun connect-tag (mapper locator)
  (connect-all mapper
   (("/tags"
     (lambda (params req)
       (declare (ignore params))
       (mita.db:with-connection (conn (make-db req locator))
         (html-response (mita.web.html:tags conn)))))
    (("/api/tags/_create" :method :post)
     (lambda (params req)
       (declare (ignore params))
       (mita.db:with-connection (conn (make-db req locator))
         (let ((name (q req "name")))
           (mita.tag:create-tag conn name)
           (json-response (jsown:new-js))))))
    ("/api/tags"
     (lambda (params req)
       (declare (ignore params))
       (mita.db:with-connection (conn (make-db req locator))
         (json-response (mita.tag:load-tags conn)))))
    (("/api/tags/:tag-id" :method :delete)
     (lambda (params req)
       (mita.db:with-connection (conn (make-db req locator))
         (let ((tag-id (ensure-uuid-short (getf params :tag-id))))
           (mita.tag:delete-tag conn tag-id)
           (json-response (jsown:new-js))))))
    (("/api/tags/:tag-id" :method :put)
     (lambda (params req)
       (mita.db:with-connection (conn (make-db req locator))
         (let ((tag-id (ensure-uuid-short (getf params :tag-id)))
               (name (q req "name")))
           (when-let ((tag (mita.tag:load-tag-by-id conn tag-id)))
             (mita.tag:update-tag-name conn tag name))
           (json-response (jsown:new-js))))))
    (("/api/tags/:tag-id/contents")
     (lambda (params req)
       (mita.db:with-connection (conn (make-db req locator))
         (let ((tag-id (ensure-uuid-short (getf params :tag-id))))
           (if-let ((tag (mita.tag:load-tag-by-id conn tag-id)))
             (json-response
              (mapcar #'mita.web.jsown:as-content
                      (mita.tag:tag-contents conn tag)))
             (json-response (jsown:new-js) :status-code 404))))))

    ("/api/albumTags/:album-id"
     (lambda (params req)
       (mita.db:with-connection (conn (make-db req locator))
         (let ((album-id (ensure-uuid-short (getf params :album-id))))
           (if-let ((album (mita.album:load-album-by-id conn album-id)))
             (json-response (mita.tag:content-tags conn album))
             (json-response (jsown:new-js) :status-code 404))))))
    (("/api/albumTags/:album-id" :method :put)
     (lambda (params req)
       (mita.db:with-connection (conn (make-db req locator))
         (let ((album-id
                (ensure-uuid-short (getf params :album-id)))
               (nullable-tag-id-list
                (mapcar #'ensure-uuid-short
                        (cdr (assoc "tag-id-list"
                                    (lack.request:request-body-parameters
                                     req)
                                    :test #'string=)))))
           (when nullable-tag-id-list
             (when-let ((album (mita.album:load-album-by-id conn album-id)))
               (mita.tag:update-content-tags
                conn album (remove nil nullable-tag-id-list))))))
       (json-response (jsown:new-js)))))))

;;;

(defun make-middleware (locator spec &key serve-image-p)
  (let ((mapper (myway:make-mapper)))
    (connect-album mapper spec)
    (connect-view mapper locator)
    (connect-home mapper)
    (connect-tag mapper locator)
    (connect-dir mapper spec)
    (when serve-image-p
      (connect-image mapper spec))
    (mapper->middleware mapper)))
