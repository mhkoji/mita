(defpackage :mita.web.clack
  (:use :cl :mita.clack.util)
  (:export :make-middleware)
  (:import-from :mita.web.app
                :get-db)
  (:import-from :alexandria
                :when-let
                :when-let*
                :if-let))
(in-package :mita.web.clack)

;;;

(defun connect-dir (mapper spec)
  (connect-all mapper
   (("/dir/*"
     (lambda (params req)
       (mita.web.app:dir-serve
        spec req (car (getf params :splat))
        :on-folder
        (lambda (folder files)
          (html-response (mita.web.html:dir folder files)))
        :on-file
        (lambda (full-path)
          `(200 () ,(parse-namestring full-path)))
        :on-not-found (lambda ()))))
    (("/dir/*" :method :post)
     (lambda (params req)
       (let ((parent-dir (car (getf params :splat)))
             (files (mapcar (lambda (item)
                              (destructuring-bind
                                  (name stream path content-type) item
                                (declare (ignore name content-type))
                                (list path stream)))
                            (lack.request:request-body-parameters req))))
         (mita.web.app:dir-add spec req parent-dir files))
       `(200 (:content-type "text/plain") ("OK"))))
    (("/api/dir/*" :method :delete)
     (lambda (params req)
       (let ((parent-dir (car (getf params :splat))))
         (mita.web.app:dir-delete spec req parent-dir))
       (json-response (jsown:new-js))))

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
             (mita.db:with-connection (conn (get-db spec req))
               (mita.album:load-albums conn offset limit))
           (let ((format-str "/albums?offset=~A&limit=~A"))
             (html-response
              (mita.web.html:albums
               (if full-loaded-p (butlast albums) albums)
               (when (< 0 offset)
                 (format nil format-str (max (- offset limit) 0) limit))
               (when full-loaded-p
                 (format nil format-str (+ offset limit) limit)))))))))
    ("/albums/:album-id"
     (lambda (params req)
       (mita.db:with-connection (conn (get-db spec req))
         (let ((album-id (ensure-uuid-short (getf params :album-id))))
           (if-let ((album (mita.album:load-album-by-id conn album-id)))
             (html-response (mita.web.html:album conn album))
             (html-response (mita.web.html:not-found)
                            :status-code 404)))))))))

(defun connect-view (mapper spec)
  (connect-all mapper
   (("/view/album/:album-id"
     (lambda (params req)
       (mita.db:with-connection (conn (get-db spec req))
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

(defun connect-tag (mapper spec)
  (connect-all mapper
   (("/tags"
     (lambda (params req)
       (declare (ignore params))
       (mita.db:with-connection (conn (get-db spec req))
         (html-response (mita.web.html:tags conn)))))
    (("/api/tags/_create" :method :post)
     (lambda (params req)
       (declare (ignore params))
       (mita.db:with-connection (conn (get-db spec req))
         (let ((name (q req "name")))
           (mita.tag:do-create-tag (push-tag conn)
             (push-tag name))
           (json-response (jsown:new-js))))))
    ("/api/tags"
     (lambda (params req)
       (declare (ignore params))
       (mita.db:with-connection (conn (get-db spec req))
         (json-response (mita.tag:load-tags conn)))))
    (("/api/tags/:tag-id" :method :delete)
     (lambda (params req)
       (mita.db:with-connection (conn (get-db spec req))
         (let ((tag-id (ensure-uuid-short (getf params :tag-id))))
           (mita.tag:delete-tag conn tag-id)
           (json-response (jsown:new-js))))))
    (("/api/tags/:tag-id" :method :put)
     (lambda (params req)
       (mita.db:with-connection (conn (get-db spec req))
         (let ((tag-id (ensure-uuid-short (getf params :tag-id)))
               (name (q req "name")))
           (when-let ((tag (mita.tag:load-tag-by-id conn tag-id)))
             (mita.tag:update-tag-name conn tag name))
           (json-response (jsown:new-js))))))
    (("/api/tags/:tag-id/contents")
     (lambda (params req)
       (mita.db:with-connection (conn (get-db spec req))
         (let ((tag-id (ensure-uuid-short (getf params :tag-id))))
           (if-let ((tag (mita.tag:load-tag-by-id conn tag-id)))
             (json-response
              (mapcar #'mita.web.jsown:as-content
                      (mita.tag:tag-contents conn tag)))
             (json-response (jsown:new-js) :status-code 404))))))

    ("/api/albumTags/:album-id"
     (lambda (params req)
       (mita.db:with-connection (conn (get-db spec req))
         (let ((album-id (ensure-uuid-short (getf params :album-id))))
           (if-let ((album (mita.album:load-album-by-id conn album-id)))
             (json-response (mita.tag:content-tags conn album))
             (json-response (jsown:new-js) :status-code 404))))))
    (("/api/albumTags/:album-id" :method :put)
     (lambda (params req)
       (mita.db:with-connection (conn (get-db spec req))
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

(defun make-middleware (spec &key serve-image-p)
  (let ((mapper (myway:make-mapper)))
    (connect-album mapper spec)
    (connect-view mapper spec)
    (connect-home mapper)
    (connect-tag mapper spec)
    (connect-dir mapper spec)
    (when serve-image-p
      (connect-image mapper spec))
    (mapper->middleware mapper)))
