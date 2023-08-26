(defpackage :mita.web.server.hunchentoot
  (:use :cl)
  (:export :*service*
           :warmup
           :start
           :stop)
  (:import-from :mita.util.threading
                :->))
(in-package :mita.web.server.hunchentoot)

(defgeneric dispatch (dispatcher method path))

(defclass acceptor (hunchentoot:acceptor)
  ((dispatcher :initarg :dispatcher
               :reader acceptor-dispatcher)))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor acceptor)
                                                  request)
  (or (dispatch (acceptor-dispatcher acceptor)
                (hunchentoot:request-method request)
                (hunchentoot:script-name request))
      ;; Serve static files by the base acceptor class.
      (call-next-method)))

;;;

(defmethod dispatch ((mapper myway:mapper) method path)
  (multiple-value-bind (resp found-p)
      (myway:dispatch mapper path :method method)
    (when found-p
      resp)))

(defmacro gen-mapper (&rest clauses)
  `(let ((mapper (myway:make-mapper)))
     (progn
       ,@(mapcar (lambda (cls)
                   (destructuring-bind (method url params &body body) cls
                     `(myway:connect mapper ,url
                                     ,(if (null params)
                                          `(lambda (p)
                                             (declare (ignore p))
                                             ,@body)
                                          `(lambda ,params ,@body))
                                     :method ,method)))
                 clauses)
       mapper)))

(defun query-params* ()
  (-> (hunchentoot:query-string*)
      (quri:url-decode-params :lenient t)))

(defun body-jsown* ()
  (-> (hunchentoot:raw-post-data :force-binary t)
      (babel:octets-to-string :encoding :utf-8)
      (jsown:parse)))

(defmacro do-static (&body body)
  `(progn
     (setf (hunchentoot:header-out "cache-control") "max-age=31536000")
     (hunchentoot:handle-static-file ,@body)))

(defmacro do-html (&body body)
  `(progn
     (setf (hunchentoot:content-type*) "text/html")
     ,@body))

(defmacro do-json (&body body)
  `(progn
     (setf (hunchentoot:content-type*) "application/json")
     ,@body))

(defun create-mapper (service)
  (gen-mapper
   (:get "/" ()
    (hunchentoot:redirect "/folder/"))

   (:get "/folder*" (params)
    (mita.web.folder:service-path-content
     service (car (getf params :splat))
     :on-file
     (lambda (path)
       (do-static path))
     :on-folder
     (lambda (detail)
       (do-html
        (mita.web.html:folder (mita.web.json:folder-detail detail))))
     :on-not-found (lambda () nil)))

   (:get "/view*" (params)
    (mita.web.folder:service-folder-images
     service (car (getf params :splat))
     :on-found
     (lambda (images)
       (do-html
        (mita.web.html:view images)))
     :on-not-found (lambda () nil)))

   (:get "/tags" ()
    (do-html
     (mita.web.html:tags)))
   (:get "/api/tags" ()
    (do-json
     (let ((tags (mita.web.tag:service-list-tags service)))
       (mita.web.json:tag-list tags))))
   (:post "/api/tags/_create" ()
    (do-json
     (let ((qp (query-params*)))
       (let ((tag (mita.web.tag:service-tag-add
                   service
                   (cdr (assoc "name" qp :test #'string=)))))
         (mita.web.json:tag tag)))))
   (:get "/api/tags/:tag-id/folders" (params)
    (do-json
     (let ((overview-list (mita.web.folder:service-tag-folders
                           service
                           (getf params :tag-id))))
       (mita.web.json:folder-overview-list overview-list))))

   (:get "/api/folder/tags" ()
    (do-json
     (let ((qp (query-params*)))
       (let ((tags (mita.web.folder:service-folder-tags
                    service
                    (cdr (assoc "path" qp :test #'string=)))))
         (mita.web.json:tag-list tags)))))
   (:put "/api/folder/tags" ()
    (do-json
     (let ((qp (query-params*))
           (bj (body-jsown*)))
       (mita.web.folder:service-folder-set-tags
        service
        (cdr (assoc "path" qp :test #'string=))
        (jsown:val bj "tag-id-list")))
      (mita.web.json:empty)))))

;;;

(defclass taskmaster (hunchentoot:one-thread-per-connection-taskmaster)
  (process))

(defmethod hunchentoot:execute-acceptor ((taskmaster taskmaster))
  (let ((process (call-next-method)))
    (setf (slot-value taskmaster 'process) process)
    process))
  
(defvar *acceptor* nil)

(defun stop ()
  (when (and *acceptor*)
    (ignore-errors
     (hunchentoot:stop *acceptor*))))

(defun start (&key (service (mita.web.service:make-service))
                   (port 5000)
                   (document-root (merge-pathnames
                                   "www/"
                                   (asdf:system-source-directory :mita-web)))
                   use-webpack-dev-server-p)
  (stop)
  (setq mita.web.html:*base-gen-path*
        (if use-webpack-dev-server-p
            "http://localhost:9000/"
            "/static/gen/"))
  (let ((taskmaster (make-instance 'taskmaster)))
    (setq *acceptor* (make-instance 'acceptor
                                    :port port
                                    :dispatcher (create-mapper service)
                                    :taskmaster taskmaster
                                    :document-root document-root))
    (hunchentoot:start *acceptor*)
    (slot-value taskmaster 'process)))

(defun warmup (service)
  (mita.web.service:service-warmup service))
