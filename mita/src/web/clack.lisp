(defpackage :mita.web.clack
  (:use :cl :mita.clack.util)
  (:export :make-middleware)
  (:import-from :mita.web.dep
                :get-db)
  (:import-from :alexandria
                :when-let
                :when-let*
                :if-let))
(in-package :mita.web.clack)

;;;

(defun connect-dir (mapper dep)
  (connect-all mapper
   (("/dir/*"
     (lambda (params req)
       (mita.web.file:get-file
        dep req (car (getf params :splat))
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
         (mita.web.file:add-files dep req parent-dir files))
       `(200 (:content-type "text/plain") ("OK"))))
    (("/api/dir/*" :method :delete)
     (lambda (params req)
       (let ((parent-dir (car (getf params :splat))))
         (mita.web.file:delete-file dep req parent-dir))
       (json-response (jsown:new-js))))

    (("/api/dir/add-albums" :method :post)
     (lambda (params req)
       (declare (ignore params))
       (mita.web.file:add-albums dep req (q req "path"))
       (json-response (jsown:new-js)))))))

(defun connect-image (mapper dep)
  (connect-all mapper
   (("/images/:image-id"
     (lambda (params req)
       (mita.web.image:find-image
        dep req (getf params :image-id)
        :on-found (lambda (path) `(200 () ,path))
        :on-not-found (lambda () nil)))))))

(defun connect-album (mapper dep)
  (connect-all mapper
   (("/albums"
     (lambda (params req)
       (declare (ignore params))
       (let ((offset (ensure-integer (q req "offset") 0))
             (limit (ensure-integer (q req "limit") 50)))
         (mita.load-albums:run (get-db dep req) offset limit
          :on-loaded
          (lambda (albums prev-offset next-offset)
            (html-response
             (let ((format-str "/albums?offset=~A&limit=~A"))
               (mita.web.html:albums
                albums
                (when prev-offset
                  (format nil format-str prev-offset limit))
                (when next-offset
                  (format nil format-str next-offset limit))))))))))
    ("/albums/:album-id"
     (lambda (params req)
       (mita.web.album:load-album-by-id
        dep req (ensure-uuid-short (getf params :album-id))
        :on-found
        (lambda (album images)
          (html-response (mita.web.html:album album images)))
        :on-not-found
        (lambda ()
          (html-response (mita.web.html:not-found) :status-code 404))))))))

(defun connect-view (mapper dep)
  (connect-all mapper
   (("/view/album/:album-id"
     (lambda (params req)
       (mita.web.album:load-album-by-id
        dep req (ensure-uuid-short (getf params :album-id))
        :on-found
        (lambda (album images)
          (declare (ignore album))
          (html-response (mita.web.html:view images)))
        :on-not-found
        (lambda ()
          (html-response (mita.web.html:not-found) :status-code 404)))))
    ("/view/dir/*"
     (lambda (params req)
       (mita.web.file:get-file
        dep req (car (getf params :splat))
        :on-folder
        (lambda (folder files)
          (declare (ignore folder))
          (html-response (mita.web.html:view (remove-if #'mita.file:folder-p
                                                        files))))
        :on-file
        (lambda (full-path)
          `(200 () ,(parse-namestring full-path)))
        :on-not-found (lambda ())))))))
  
(defun connect-home (mapper)
  (connect-all mapper
   (("/"
     (lambda (params req)
       (declare (ignore params req))
       (html-response (mita.web.html:home)))))))

(defun connect-tag (mapper dep)
  (connect-all mapper
   (("/tags"
     (lambda (params req)
       (declare (ignore params))
       (mita.db:with-connection (conn (get-db dep req))
         (html-response (mita.web.html:tags conn)))))
    (("/api/tags/_create" :method :post)
     (lambda (params req)
       (declare (ignore params))
       (mita.db:with-connection (conn (get-db dep req))
         (let ((name (q req "name")))
           (mita.tag:do-create-tag (push-tag conn)
             (push-tag name))
           (json-response (jsown:new-js))))))
    ("/api/tags"
     (lambda (params req)
       (declare (ignore params))
       (mita.db:with-connection (conn (get-db dep req))
         (json-response (mita.tag:load-tags conn)))))
    (("/api/tags/:tag-id" :method :delete)
     (lambda (params req)
       (mita.db:with-connection (conn (get-db dep req))
         (let ((tag-id (ensure-uuid-short (getf params :tag-id))))
           (mita.tag:delete-tag conn tag-id)
           (json-response (jsown:new-js))))))
    (("/api/tags/:tag-id" :method :put)
     (lambda (params req)
       (mita.db:with-connection (conn (get-db dep req))
         (let ((tag-id (ensure-uuid-short (getf params :tag-id)))
               (name (q req "name")))
           (when-let ((tag (mita.tag:load-tag-by-id conn tag-id)))
             (mita.tag:update-tag-name conn tag name))
           (json-response (jsown:new-js))))))
    (("/api/tags/:tag-id/contents")
     (lambda (params req)
       (mita.db:with-connection (conn (get-db dep req))
         (let ((tag-id (ensure-uuid-short (getf params :tag-id))))
           (if-let ((tag (mita.tag:load-tag-by-id conn tag-id)))
             (json-response
              (mapcar #'mita.web.jsown:as-content
                      (mita.tag:tag-contents conn tag)))
             (json-response (jsown:new-js) :status-code 404))))))

    ("/api/albumTags/:album-id"
     (lambda (params req)
       (mita.db:with-connection (conn (get-db dep req))
         (let ((album-id (ensure-uuid-short (getf params :album-id))))
           (if-let ((album (mita.album:load-album-by-id conn album-id)))
             (json-response (mita.tag:content-tags conn album))
             (json-response (jsown:new-js) :status-code 404))))))
    (("/api/albumTags/:album-id" :method :put)
     (lambda (params req)
       (mita.db:with-connection (conn (get-db dep req))
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

(defun make-middleware (dep &key serve-image-p)
  (let ((mapper (myway:make-mapper)))
    (connect-album mapper dep)
    (connect-view mapper dep)
    (connect-home mapper)
    (connect-tag mapper dep)
    (connect-dir mapper dep)
    (when serve-image-p
      (connect-image mapper dep))
    (mapper->middleware mapper)))
