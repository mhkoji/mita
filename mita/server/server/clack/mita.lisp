(defpackage :mita.server.clack.mita
  (:use :cl)
  (:export :make-middleware)
  (:import-from :mita.server.app
                :with-db)
  (:import-from :alexandria
                :when-let
                :when-let*
                :if-let))
(in-package :mita.server.clack.mita)

(defmethod mita.server.app:request-account-id ((req lack.request:request))
  (getf (lack.request:request-env req) :mita.util.auth.identity))

;;;

(define-condition bad-request () ())

(define-condition server-error () ())


(defun ensure-integer (obj &optional default-value)
  (cond ((null obj)
         (if (typep default-value 'integer)
             default-value
             (error 'server-error)))
        ((stringp obj)
         (handler-case (parse-integer obj)
           (error ()
             (error 'bad-request))))
        (t
         (error 'bad-request))))

(defun ensure-uuid-short (obj)
  (if (stringp obj)
      (handler-case (mita.id:parse-short obj)
        (error ()
          (error 'bad-request)))
      (error 'bad-request)))



(defun html-response (body-string &key (status-code 200))
  `(,status-code (:content-type "text/html")
                 (,body-string)))

(defun json-response (value &key (status-code 200) (success t))
  `(,status-code (:content-type "application/json")
                 (,(jsown:to-json
                    (jsown:new-js
                      ("success" (or success :f))
                      ("value" value))))))

(defun print-backtrace (&key (stream *standard-output*))
  #+sbcl
  (progn
    (format stream "Backtrace for: ~A~%"
            (sb-thread:thread-name sb-thread:*current-thread*))
    (loop for i from 0
          ;; See also: https://github.com/sbcl/sbcl/blob/dd4bcce1ca218502ca044da7596ce5953fd81d9e/src/code/debug.lisp#L385
          for (name &rest args) in (sb-debug:list-backtrace)
          ;; Should not print args because they may contain sensitive information such as a password.
          do (format stream "~&~S: ~A~%" i name))))

(defmacro with-safe-json-response (&body body)
  `(handler-case
       (or (progn ,@body)
           (json-response (jsown:new-js)
                          :success nil
                          :status-code 404))
     (error (e)
       (declare (ignore e))
       (print-backtrace)
       (json-response (jsown:new-js)
                      :success nil
                      :status-code 500))))

(defmacro with-safe-html-response (&body body)
  `(block nil
     (handler-bind
         ((bad-request
           (lambda (c)
             (declare (ignore c))
             (print-backtrace)
             (return (html-response
                      "Bad Request"
                      :status-code 400))))
          (server-error
           (lambda (c)
             (declare (ignore c))
             (print-backtrace)
             (return (html-response
                      (mita.server.html:internal-server-error)
                      :status-code 500))))
          (error
           (lambda (c)
             (declare (ignore c))
             (print-backtrace)
             (return (html-response
                      (mita.server.html:internal-server-error)
                      :status-code 500)))))
       (or (progn ,@body)
           (html-response (mita.server.html:not-found)
                          :status-code 404)))))

(defmacro with-json-api ((db conn-holder req) &body body)
  `(with-safe-json-response
     (with-db (,db ,conn-holder ,req)
       ,@body)))

(defun q (req name)
  (let ((params (lack.request:request-parameters req)))
    (cdr (assoc name params :test #'string=))))

(defmacro connect-all (mapper arg-list)
  `(progn
     ,@(mapcar (lambda (arg)
                 (destructuring-bind (endpoint fn) arg
                   (destructuring-bind (url &rest rest)
                       (alexandria:ensure-list endpoint)
                     `(myway:connect ,mapper ,url
                                     (lambda (params)
                                       (lambda (req)
                                         (,fn params req)))
                                     ,@rest))))
               arg-list)))

(defun mapper->middleware (mapper)
  (lambda (app)
    (lambda (env)
      (or (let ((request (lack.request:make-request env)))
            (multiple-value-bind (handler foundp)
                (let ((method (lack.request:request-method request))
                      (path-info (lack.request:request-path-info request)))
                  (myway:dispatch mapper path-info :method method))
              (when foundp
                (funcall handler request))))
          (funcall app env)))))

(defun connect-page (mapper connector)
  (connect-all mapper
   (("/pages"
     (lambda (params req)
       (declare (ignore params))
       (with-safe-html-response
         (with-db (db connector req)
           (let ((pages (mita.page:load-pages db)))
             (html-response
              (mita.server.html:pages pages)))))))
    ("/pages/:page-id"
     (lambda (params req)
       (with-safe-html-response
         (with-db (db connector req)
           (when-let*
               ((page-id
                 (mita.id:parse-short-or-nil
                  (getf params :page-id)))
                (page
                 (mita.page:load-page-by-id db page-id)))
             (html-response
              (mita.server.html:page db page)))))))
    (("/api/pages/:page-id/text" :method :put)
     (lambda (params req)
       (with-json-api (db connector req)
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
            (mita.page:update-page-text db page text))))))
    (("/api/pages/_create" :method :post)
     (lambda (params req)
       (declare (ignore params))
       (with-json-api (db connector req)
         (let ((page (mita.page:create-page db)))
           (json-response
            (jsown:new-js
              ("redirect"
               (mita.server.jsown:url-for page)))))))))))

(defun connect-dir (mapper spec)
  (connect-all mapper
   (("/dir/*"
     (lambda (params req)
       (with-safe-html-response
         (mita.server.app:dir-serve
          spec req (car (getf params :splat))
          :on-found
          (lambda (file)
            (if (mita.fs:dir-p file)
                (html-response (mita.server.html:dir file))
                `(200 () ,(parse-namestring
                           (mita.fs.dir:file-full-path file)))))
          :on-not-found
          (lambda ())))))
    (("/api/dir/add-albums" :method :post)
     (lambda (params req)
       (declare (ignore params))
       (with-safe-json-response
         (mita.server.app:dir-add-albums spec req (q req "path"))
         (json-response (jsown:new-js))))))))

(defun connect-image (mapper spec)
  (connect-all mapper
   (("/images/:image-id"
     (lambda (params req)
       (with-safe-html-response
         (mita.server.app:image-serve
          spec req (getf params :image-id)
          :on-found (lambda (path)
                      `(200 () ,path))
          :on-not-found (lambda ()
                          nil))))))))

(defun connect-album (mapper connector)
  (connect-all mapper
   (("/albums"
     (lambda (params req)
       (declare (ignore params))
       (with-safe-html-response
         (with-db (db connector req)
           (let ((offset (ensure-integer (q req "offset") 0))
                 (limit (ensure-integer (q req "limit") 50)))
             (let* ((albums (mita.album:load-albums db offset (1+ limit)))
                    (full-loaded-p (= (length albums) (1+ limit)))
                    (format-str "/albums?offset=~A&limit=~A"))
               (html-response
                (mita.server.html:albums
                 (if full-loaded-p (butlast albums) albums)
                 (when (< 0 offset)
                   (format nil format-str (max (- offset limit) 0) limit))
                 (when full-loaded-p
                   (format nil format-str (+ offset limit) limit))))))))))
    ("/albums/:album-id"
     (lambda (params req)
       (with-safe-html-response
         (with-db (db connector req)
           (let ((album-id (ensure-uuid-short (getf params :album-id))))
             (when-let ((album (mita.album:load-album-by-id db album-id)))
               (html-response (mita.server.html:album db album)))))))))))

(defun connect-view (mapper connector)
  (connect-all mapper
   (("/view/album/:album-id"
     (lambda (params req)
       (with-safe-html-response
         (with-db (db connector req)
           (let ((album-id (ensure-uuid-short (getf params :album-id))))
             (when-let ((album (mita.album:load-album-by-id db album-id)))
               (html-response (mita.server.html:view
                               (mita.album:album-images db album))))))))))))
  
(defun connect-home (mapper)
  (connect-all mapper
   (("/"
     (lambda (params req)
       (declare (ignore params req))
       (with-safe-html-response
         (html-response (mita.server.html:home))))))))

(defun connect-tag (mapper connector)
  (connect-all mapper
   (("/tags"
     (lambda (params req)
       (declare (ignore params))
       (with-safe-html-response
         (with-db (db connector req)
           (html-response (mita.server.html:tags db))))))
    (("/api/tags/_create" :method :post)
     (lambda (params req)
       (declare (ignore params))
       (with-json-api (db connector req)
         (let ((name (q req "name")))
           (mita.tag:create-tag db name)
           (json-response (jsown:new-js))))))
    ("/api/tags"
     (lambda (params req)
       (declare (ignore params))
       (with-json-api (db connector req)
         (json-response (mita.tag:load-tags db)))))
    (("/api/tags/:tag-id" :method :delete)
     (lambda (params req)
       (with-json-api (db connector req)
         (let ((tag-id (ensure-uuid-short (getf params :tag-id))))
           (mita.tag:delete-tag db tag-id)
           (json-response (jsown:new-js))))))
    (("/api/tags/:tag-id" :method :put)
     (lambda (params req)
       (with-json-api (db connector req)
         (let ((tag-id (ensure-uuid-short (getf params :tag-id)))
               (name (q req "name")))
           (when-let ((tag (mita.tag:load-tag-by-id db tag-id)))
             (mita.tag:update-tag-name db tag name)
             (json-response (jsown:new-js)))))))
    (("/api/tags/:tag-id/contents")
     (lambda (params req)
       (with-json-api (db connector req)
         (let ((tag-id (ensure-uuid-short (getf params :tag-id))))
           (when-let ((tag (mita.tag:load-tag-by-id db tag-id)))
             (json-response (mapcar
                             #'mita.server.jsown:as-content
                             (mita.tag:tag-contents db tag))))))))

    ("/api/albumTags/:album-id"
     (lambda (params req)
       (with-json-api (db connector req)
         (let ((album-id (ensure-uuid-short (getf params :album-id))))
           (when-let ((album (mita.album:load-album-by-id db album-id)))
             (json-response (mita.tag:content-tags db album)))))))
    (("/api/albumTags/:album-id" :method :put)
     (lambda (params req)
       (with-json-api (db connector req)
         (let ((album-id
                 (ensure-uuid-short (getf params :album-id)))
               (nullable-tag-id-list
                 (mapcar #'ensure-uuid-short
                         (cdr (assoc "tag-id-list"
                                     (lack.request:request-body-parameters
                                      req)
                                     :test #'string=)))))
           (when nullable-tag-id-list
             (when-let ((album (mita.album:load-album-by-id db album-id)))
               (mita.tag:update-content-tags
                db album (remove nil nullable-tag-id-list))
               (json-response (jsown:new-js)))))))))))

;;;

(defun make-middleware (connector spec &key serve-image-p)
  (let ((mapper (myway:make-mapper)))
    (connect-album mapper connector)
    (connect-view mapper connector)
    (connect-page mapper connector)
    (connect-home mapper)
    (connect-tag mapper connector)
    (connect-dir mapper spec)
    (when serve-image-p
      (connect-image mapper spec))
    (mapper->middleware mapper)))
