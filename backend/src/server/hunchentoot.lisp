(defpackage :mita.server.hunchentoot
  (:use :cl)
  (:export :*static-root*
           :start
           :stop)
  (:import-from :mita.util.threading
                :->))
(in-package :mita.server.hunchentoot)

(defvar *mapper* nil)

(setq *mapper* (myway:make-mapper))

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
     (mita.html:folder (mita.server.json:folder-detail detail)))
   :on-not-found (lambda () nil)))

(connect! (params "/view*")
  (mita.main:show-viewer (car (getf params :splat))
   :on-found
   (lambda (v)
     (setf (hunchentoot:content-type*) "text/html")
     (mita.html:view (mita.server.json:viewer v)))
   :on-not-found (lambda () nil)))

(connect! (_ "/tags")
  (setf (hunchentoot:content-type*) "text/html")
  (mita.html:tags))

(connect! (_ "/api/tags")
  (setf (hunchentoot:content-type*) "application/json")
  (let ((tags (mita.main:list-tags)))
    (mita.server.json:tag-list tags)))

(connect! (_ "/api/tags/_create" :method :post)
  (setf (hunchentoot:content-type*) "application/json")
  (let ((qp (query-params*)))
    (let ((tag (mita.main:tag-add
                (cdr (assoc "name" qp :test #'string=)))))
      (mita.server.json:tag tag))))

(connect! (params "/api/tags/:tag-id/folders")
  (setf (hunchentoot:content-type*) "application/json")
  (let ((overview-list (mita.main:tag-folders (getf params :tag-id))))
    (mita.server.json:folder-overview-list overview-list)))

(connect! (_ "/api/folder/tags")
  (setf (hunchentoot:content-type*) "application/json")
  (let ((qp (query-params*)))
    (let ((tags (mita.main:folder-tags
                 (cdr (assoc "path" qp :test #'string=)))))
      (mita.server.json:tag-list tags))))

(connect! (_ "/api/folder/tags" :method :put)
  (setf (hunchentoot:content-type*) "application/json")
  (let ((qp (query-params*))
        (bj (body-jsown*)))
    (mita.main:folder-set-tags
     (cdr (assoc "path" qp :test #'string=))
     (jsown:val bj "tag-id-list")))
  (mita.server.json:empty))

;;;

(defclass acceptor (hunchentoot:acceptor)
  ())

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor acceptor)
                                                  request)
  (multiple-value-bind (resp found-p)
      (let ((path (hunchentoot:script-name request))
            (method (hunchentoot:request-method request)))
        (myway:dispatch *mapper* path :method method))
    (if (and resp found-p)
        resp
        ;; Serve static files by the base acceptor class.
        (call-next-method))))

(defvar *acceptor* nil)

(defun stop ()
  (when (and *acceptor*)
    (ignore-errors
     (hunchentoot:stop *acceptor*))))
  
(defun start (&key (port 5000)
                   (document-root (asdf:system-source-directory :mita)))
  (stop)
  (setq *acceptor* (make-instance 'acceptor
                                  :port port
                                  :document-root document-root))
  (hunchentoot:start *acceptor*))
