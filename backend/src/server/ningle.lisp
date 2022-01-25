(defpackage :mita.server.ningle
  (:use :cl)
  (:export :*static-root*
           :start)
  (:import-from :mita.util.threading
                :->
                :->>))
(in-package :mita.server.ningle)

;;;

(defvar *app* nil)

(setq *app* (make-instance 'ningle:app))

(setf
 (ningle:route *app* "/")
 (lambda (params)
   (declare (ignore params))
   '(302 (:location "/folder/") nil)))

(setf
 (ningle:route *app* "/folder*")
 (lambda (params)
   (mita.main:show-path (cadr (assoc :splat params))
    :on-file
    (lambda (path)
      `(200 (:cache-control "max-age=31536000")
            ,path))
    :on-folder
    (lambda (detail)
      `(200 (:content-type "text/html")
            (,(mita.html:folder (folder-detail->jsown detail)))))
    :on-not-found
    (lambda ()
      (ningle:not-found *app*)))))

(setf
 (ningle:route *app* "/view*")
 (lambda (params)
   (mita.main:folder-image-files (cadr (assoc :splat params))
    :on-found
    (lambda (files)
      `(200 (:content-type "text/html")
            (,(mita.html:view (mapcar #'file->jsown files)))))
    :on-not-found
    (lambda ()
      (ningle:not-found *app*)))))


(setf
 (ningle:route *app* "/tags")
 (lambda (params)
   (declare (ignore params))
   `(200 (:content-type "text/html")
         (,(mita.html:tags)))))

(setf
 (ningle:route *app* "/api/tags")
 (lambda (params)
   (declare (ignore params))
   (let ((tags (mita.main:list-tags)))
     `(200 (:content-type "application/json")
           (,(jsown:to-json (mapcar #'tag->jsown tags)))))))

(setf
 (ningle:route *app* "/api/folder/tags")
 (lambda (params)
   (let ((tags (mita.main:folder-tags
                (cdr (assoc "path" params :test #'string=)))))
     `(200 (:content-type "application/json")
           (,(jsown:to-json (mapcar #'tag->jsown tags)))))))

(setf
 (ningle:route *app* "/api/folder/tags" :method :put)
 (lambda (params)
   (mita.main:folder-set-tags
    (cdr (assoc "path" params :test #'string=))
    (cdr (assoc "tag-id-list" params :test #'string=)))
   `(200 (:content-type "application/json")
         (,(jsown:to-json (jsown:new-js))))))

(setf
 (ningle:route *app* "/api/tags/_create" :method :post)
 (lambda (params)
   (let ((tag (mita.main:tag-add
               (cdr (assoc "name" params :test #'string=)))))
     `(200 (:content-type "application/json")
           (,(jsown:to-json (tag->jsown tag)))))))

(setf
 (ningle:route *app* "/api/tags/:tag-id/folders")
 (lambda (params)
   (let ((overview-list (mita.main:tag-folders
                         (cdr (assoc :tag-id params :test #'string=)))))
     `(200 (:content-type "application/json")
           (,(jsown:to-json
              (mapcar #'folder-overview->jsown overview-list)))))))

;;;

(defvar *static-root*
  (merge-pathnames "static/" (asdf:system-source-directory :mita)))

(defvar *handler*
  nil)

(defun start (&key (port 5000)
                   (use-thread t))
  (when *handler*
    (clack:stop *handler*))
  (setq *handler*
        (clack:clackup
         (lack:builder
          (:static :path "/static/" :root *static-root*)

          #+nil
          (lambda (next)
            (lambda (env)
              (log:info env)
              (funcall next env)))

          *app*)
         :address "0.0.0.0"
         :use-thread use-thread
         :port port)))
