(defpackage :mita.server.hunchentoot
  (:use :cl)
  (:export :*static-root*
           :start
           :stop)
  (:import-from :mita.util.threading
                :->))
(in-package :mita.server.hunchentoot)

(defun file->jsown (file)
  (jsown:new-js
    ("path" (namestring (mita.view:file-path file)))
    ("url"  (format nil "/folder/~A" (mita.view:file-path file)))))

(defun folder-overview->jsown (overview)
  (let ((path (mita.view:folder-overview-path overview)))
    (jsown:new-js
      ("path" (namestring path))
      ("url" (format nil "/folder/~A" path))
      ("thumbnail" (let ((file (mita.view:folder-overview-thumbnail-file
                                overview)))
                     (if file (file->jsown file) :null))))))

(defun folder-detail->jsown (detail)
  (jsown:new-js
    ("path"
     (namestring (mita.view:folder-detail-path detail)))
    ("files"
     (mapcar #'file->jsown
             (mita.view:folder-detail-file-list detail)))
    ("folders"
     (mapcar #'folder-overview->jsown
             (mita.view:folder-detail-folder-overview-list detail)))))

(defun tag->jsown (tag)
  (jsown:new-js
    ("id" (format nil "~A" (mita.tag:tag-id tag)))
    ("name" (mita.tag:tag-name tag))))

;;;

(defvar *static-root*
  (merge-pathnames "static/" (asdf:system-source-directory :mita)))

(defvar *mapper* nil)

(setq *mapper* (myway:make-mapper))

(defun not-found ()
  (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
  (hunchentoot:abort-request-handler))

(defun query-params* ()
  (-> (hunchentoot:query-string*)
      (quri:url-decode-params :lenient t)))

(defun body-jsown* ()
  (-> (hunchentoot:raw-post-data :force-binary t)
      (babel:octets-to-string :encoding :utf-8)
      (jsown:parse)))

(defmacro connect! ((params url &rest args) &body body)
  `(myway:connect *mapper* ,url
                  ,(if (eq params '_)
                       `(lambda (p)
                          (declare (ignore p))
                          ,@body)
                       `(lambda (,params) ,@body))
                  ,@args))

(connect! (params "/static/*")
  (let ((path (merge-pathnames
               (car (getf params :splat)) *static-root*)))
    (if (uiop/filesystem:file-exists-p path)
        (hunchentoot:handle-static-file path)
        (not-found))))

(connect! (_ "/")
  (hunchentoot:redirect "/folder/"))

(connect! (params "/folder*")
  (mita.main:show-path (car (getf params :splat))
   :on-file
   (lambda (path)
     (setf (hunchentoot:header-out "cache-control") "max-age=31536000")
     (hunchentoot:handle-static-file path))
   :on-folder
   (lambda (detail)
     (setf (hunchentoot:content-type*) "text/html")
     (mita.html:folder (folder-detail->jsown detail)))
   :on-not-found
   (lambda ()
     (not-found))))

(connect! (params "/view*")
  (mita.main:folder-image-files (car (getf params :splat))
  :on-found
  (lambda (files)
    (setf (hunchentoot:content-type*) "text/html")
    (mita.html:view (mapcar #'file->jsown files)))
  :on-not-found
  (lambda ()
    (not-found))))

(connect! (_ "/tags")
  (setf (hunchentoot:content-type*) "text/html")
  (mita.html:tags))

(connect! (_ "/api/tags")
  (setf (hunchentoot:content-type*) "application/json")
  (let ((tags (mita.main:list-tags)))
    (jsown:to-json (mapcar #'tag->jsown tags))))

(connect! (_ "/api/tags/_create" :method :post)
  (setf (hunchentoot:content-type*) "application/json")
  (let ((qp (query-params*)))
    (let ((tag (mita.main:tag-add
                (cdr (assoc "name" qp :test #'string=)))))
      (jsown:to-json (tag->jsown tag)))))

(connect! (params "/api/tags/:tag-id/folders")
  (setf (hunchentoot:content-type*) "application/json")
  (let ((overview-list (mita.main:tag-folders (getf params :tag-id))))
    (jsown:to-json (mapcar #'folder-overview->jsown overview-list))))

(connect! (_ "/api/folder/tags")
  (setf (hunchentoot:content-type*) "application/json")
  (let ((qp (query-params*)))
    (let ((tags (mita.main:folder-tags
                 (cdr (assoc "path" qp :test #'string=)))))
      (jsown:to-json (mapcar #'tag->jsown tags)))))

(connect! (_ "/api/folder/tags" :method :put)
  (setf (hunchentoot:content-type*) "application/json")
  (let ((qp (query-params*))
        (bj (body-jsown*)))
    (mita.main:folder-set-tags
     (cdr (assoc "path" qp :test #'string=))
     (jsown:val bj "tag-id-list")))
  (jsown:to-json (jsown:new-js)))

;;;

(defclass acceptor (hunchentoot:acceptor)
  ())

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor acceptor)
                                                  request)
  (let ((path (hunchentoot:script-name request))
        (method (hunchentoot:request-method request)))
    (multiple-value-bind (resp found-p)
        (myway:dispatch *mapper* path :method method)
      (if found-p
          resp
          (not-found)))))

(defvar *acceptor* nil)

(defun stop ()
  (when *acceptor*
    (hunchentoot:stop *acceptor*)))
  
(defun start (&key (port 5000))
  (stop)
  (setq *acceptor* (make-instance 'acceptor :port port))
  (hunchentoot:start *acceptor*))
