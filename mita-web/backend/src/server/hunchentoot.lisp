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

(defun body-yason* ()
  (-> (hunchentoot:raw-post-data :force-binary t)
      (babel:octets-to-string :encoding :utf-8)
      (yason:parse)))

(defvar *service* nil)

(defvar *mapper*
  (gen-mapper
   (:get "/" ()
    (hunchentoot:redirect "/folder/"))

   (:get "/file*" (params)
     (mita.web:service-file
      *service* (car (getf params :splat))
      :on-found
      (lambda (path)
        (setf (hunchentoot:header-out "cache-control")
              "max-age=31536000")
        (hunchentoot:handle-static-file path))
      :on-not-found (lambda () nil)))
   
   (:get "/folder*" (params)
     (mita.web:service-folder
      *service* (car (getf params :splat))
      :on-found
      (lambda  (detail)
        (mita.web.html:folder
         (mita.web.json:folder-detail detail)))
      :on-not-found (lambda () nil)))

   (:get "/view*" (params)
    (mita.web:service-folder-images
     *service* (car (getf params :splat))
     :on-found
   (lambda (images)
     (setf (hunchentoot:content-type*) "text/html")
     (mita.web.html:view (mita.web.json:viewer images)))
   :on-not-found (lambda () nil)))

   (:get "/tags" ()
    (setf (hunchentoot:content-type*) "text/html")
    (mita.web.html:tags))
   (:get "/api/tags" ()
    (setf (hunchentoot:content-type*) "application/json")
    (let ((tags (mita.web:service-list-tags *service*)))
      (mita.web.json:tag-list tags)))
   (:post "/api/tags/_create" ()
    (setf (hunchentoot:content-type*) "application/json")
    (let ((qp (query-params*)))
      (let ((tag (mita.web:service-tag-add
                  *service*
                  (cdr (assoc "name" qp :test #'string=)))))
        (mita.web.json:tag tag))))
   (:get "/api/tags/:tag-id/folders" (params)
    (setf (hunchentoot:content-type*) "application/json")
    (let ((overview-list (mita.web:service-tag-folders
                          *service*
                          (getf params :tag-id))))
      (mita.web.json:folder-overview-list overview-list)))

   (:get "/api/folder/tags" ()
    (setf (hunchentoot:content-type*) "application/json")
    (let ((qp (query-params*)))
      (let ((tags (mita.web:service-folder-tags
                   *service*
                   (cdr (assoc "path" qp :test #'string=)))))
        (mita.web.json:tag-list tags))))
   (:put "/api/folder/tags" ()
    (setf (hunchentoot:content-type*) "application/json")
    (let ((qp (query-params*))
          (by (body-yason*)))
      (mita.web:service-folder-set-tags
       *service*
       (cdr (assoc "path" qp :test #'string=))
       (gethash "tag-id-list" by)))
    (mita.web.json:empty))))

;;;

(defvar *acceptor* nil)

(defun stop ()
  (when (and *acceptor*)
    (ignore-errors
     (hunchentoot:stop *acceptor*))))

(defun start (&key (port 5000)
                   (document-root (merge-pathnames
                                   "www/"
                                   (asdf:system-source-directory :mita-web)))
                   use-webpack-dev-server-p)
  (stop)
  (setq mita.web.html:*base-gen-path*
        (if use-webpack-dev-server-p
            "http://localhost:9000/"
            "/static/gen/"))
  (setq *acceptor* (make-instance 'acceptor
                                  :port port
                                  :dispatcher *mapper*
                                  :document-root document-root))
  (hunchentoot:start *acceptor*))

(defun warmup ()
  (mita.web:service-warmup *service*))
