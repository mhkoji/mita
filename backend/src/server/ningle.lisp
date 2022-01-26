(defpackage :mita.server.ningle
  (:use :cl)
  (:export :*static-root*
           :start)
  (:import-from :mita.util.threading
                :->
                :->>))
(in-package :mita.server.ningle)

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
            (,(mita.html:folder
               (mita.server.json:folder-detail detail)))))
    :on-not-found
    (lambda ()
      (ningle:not-found *app*)))))

(setf
 (ningle:route *app* "/view*")
 (lambda (params)
   (mita.main:show-viewer (cadr (assoc :splat params))
    :on-found
    (lambda (v)
      `(200 (:content-type "text/html")
            (,(mita.html:view (mita.server.json:viewer v)))))
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
           (,(mita.server.json:tag-list tags))))))

(setf
 (ningle:route *app* "/api/folder/tags")
 (lambda (params)
   (let ((tags (mita.main:folder-tags
                (cdr (assoc "path" params :test #'string=)))))
     `(200 (:content-type "application/json")
           (,(mita.server.json:tag-list tags))))))

(setf
 (ningle:route *app* "/api/folder/tags" :method :put)
 (lambda (params)
   (mita.main:folder-set-tags
    (cdr (assoc "path" params :test #'string=))
    (cdr (assoc "tag-id-list" params :test #'string=)))
   `(200 (:content-type "application/json")
         (,(mita.server.json:empty)))))

(setf
 (ningle:route *app* "/api/tags/_create" :method :post)
 (lambda (params)
   (let ((tag (mita.main:tag-add
               (cdr (assoc "name" params :test #'string=)))))
     `(200 (:content-type "application/json")
           (,(mita.server.json:tag tag))))))

(setf
 (ningle:route *app* "/api/tags/:tag-id/folders")
 (lambda (params)
   (let ((overview-list (mita.main:tag-folders
                         (cdr (assoc :tag-id params :test #'string=)))))
     `(200 (:content-type "application/json")
           (,(mita.server.json:folder-overview-list overview-list))))))

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
