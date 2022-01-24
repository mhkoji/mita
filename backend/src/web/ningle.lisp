(defpackage :mita.web.ningle
  (:use :cl)
  (:export :*file-store*
           :*tag-store*
           :*static-root*
           :start
           :warmup))
(in-package :mita.web.ningle)

(defun file->view (file)
  (mita.web.view:make-file :path (mita.file:file-path file)))

(defun folder->overview (folder file-store)
  (mita.web.view:make-folder-overview
   :path (mita.file:file-path folder)
   :thumbnail-file
   (let ((file (first (remove-if #'mita.file:folder-p
                                 (mita.file:folder-list-files
                                  folder
                                  file-store)))))
     (when file
       (file->view file)))))

(defun folder->detail (folder file-store)
  (let ((folder-files (mita.file:folder-list-files folder file-store)))
    (mita.web.view:make-folder-detail
     :path (mita.file:file-path folder)
     :file-list
     (mapcar #'file->view
             (remove-if #'mita.file:folder-p folder-files))
     :folder-overview-list
     (mapcar (lambda (f)
               (folder->overview f file-store))
             (sort (remove-if-not #'mita.file:folder-p folder-files)
                   #'>
                   :key #'mita.file:file-created-at)))))

;;;

(defvar *file-store*
  nil)

(defvar *tag-store*
  (mita.tag:make-store
   :dir *default-pathname-defaults*))

(defun file->jsown (file)
  (jsown:new-js
    ("path" (namestring (mita.web.view:file-path file)))
    ("url"  (format nil "/folder/~A" (mita.web.view:file-path file)))))

(defun folder-overview->jsown (overview)
  (let ((path (mita.web.view:folder-overview-path overview)))
    (jsown:new-js
      ("path" (namestring path))
      ("url" (format nil "/folder/~A" path))
      ("thumbnail" (let ((file (mita.web.view:folder-overview-thumbnail-file
                                overview)))
                     (if file (file->jsown file) :null))))))

(defun folder-detail->jsown (detail)
  (jsown:new-js
    ("path"
     (namestring (mita.web.view:folder-detail-path detail)))
    ("files"
     (mapcar #'file->jsown
             (mita.web.view:folder-detail-file-list detail)))
    ("folders"
     (mapcar #'folder-overview->jsown
             (mita.web.view:folder-detail-folder-overview-list detail)))))

(defun tag->jsown (tag)
  (jsown:new-js
    ("id" (format nil "~A" (mita.tag:tag-id tag)))
    ("name" (mita.tag:tag-name tag))))

;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun simple-inserter (insert-fn)
    (lambda (acc next)
      (if (listp next)
          (funcall insert-fn acc next)
          (list next acc))))

  (defun insert-first (arg surround)
    (list* (car surround) arg (cdr surround)))

  (defun insert-last (arg surround)
    (append surround (list arg)))

  (defmacro -> (initial-form &rest forms)
    (reduce (simple-inserter #'insert-first) forms
            :initial-value initial-form))

  (defmacro ->> (initial-form &rest forms)
    (reduce (simple-inserter #'insert-last) forms
            :initial-value initial-form)))

(defvar *app* nil)

(setq *app* (make-instance 'ningle:app))

(setf
 (ningle:route *app* "/")
 (lambda (params)
   (declare (ignore params))
   '(302 (:location "/folder/") nil)))

(setf
 (ningle:route *app* "/folder*")
 (labels
     ((run (namestring)
        (let ((file (mita.file:store-make-file *file-store* namestring)))
          (cond ((uiop/filesystem:file-exists-p
                  (mita.file:file-full-path file))
                 `(200 (:cache-control "max-age=31536000")
                       ,(mita.file:file-full-path file)))
                ((uiop/filesystem:directory-exists-p
                  (mita.file:file-full-path file))
                 (assert (mita.file:folder-p file))
                 `(200 (:content-type "text/html")
                       (,(mita.web.html:folder
                          (-> (folder->detail file *file-store*)
                              (folder-detail->jsown))))))
                (t
                 `(404 (:content-type "text/plain")
                       ("Not found")))))))
   (lambda (params)
     (run (cadr (assoc :splat params))))))

(setf
 (ningle:route *app* "/view*")
 (labels
     ((run (namestring)
        (let ((file (mita.file:store-make-file *file-store* namestring)))
          (cond ((uiop/filesystem:directory-exists-p
                  (mita.file:file-full-path file))
                 (assert (mita.file:folder-p file))
                 `(200 (:content-type "text/html")
                       (,(mita.web.html:view
                          (->> (mita.file:folder-list-files file *file-store*)
                               (remove-if #'mita.file:folder-p)
                               (mapcar #'file->view)
                               (mapcar #'file->jsown))))))
                (t
                 `(404 (:content-type "text/plain")
                       ("Not found")))))))
   (lambda (params)
     (run (cadr (assoc :splat params))))))

(setf
 (ningle:route *app* "/tags")
 (lambda (params)
   (declare (ignore params))
   `(200 (:content-type "text/html")
         (,(mita.web.html:tags)))))

(setf
 (ningle:route *app* "/api/tags")
 (labels ((run ()
            (let ((tags (mita.tag:store-list-tags *tag-store*)))
              `(200 (:content-type "application/json")
                    (,(jsown:to-json (mapcar #'tag->jsown tags)))))))
   (lambda (params)
     (declare (ignore params))
     (run))))

(setf
 (ningle:route *app* "/api/folder/tags")
 (labels
     ((run (namestring)
        (let* ((folder (mita.file:store-make-file *file-store* namestring))
               (tags (mita.tag:content-tags *tag-store* folder)))
          `(200 (:content-type "application/json")
                (,(jsown:to-json (mapcar #'tag->jsown tags)))))))
   (lambda (params)
     (run (cdr (assoc "path" params :test #'string=))))))

(setf
 (ningle:route *app* "/api/folder/tags" :method :put)
 (labels
     ((run (namestring tag-id-list)
        (let ((folder (mita.file:store-make-file *file-store* namestring)))
          (mita.tag:content-tags-set *tag-store* folder tag-id-list)
          `(200 (:content-type "application/json")
                (,(jsown:to-json (jsown:new-js)))))))
   (lambda (params)
     (run (cdr (assoc "path" params :test #'string=))
          (cdr (assoc "tag-id-list" params :test #'string=))))))

(setf
 (ningle:route *app* "/api/tags/_create" :method :post)
 (labels ((run (name)
            (let ((tag (mita.tag:store-add-tag *tag-store* name)))
              `(200 (:content-type "application/json")
                    (,(jsown:to-json (tag->jsown tag)))))))
   (lambda (params)
     (run (cdr (assoc "name" params :test #'string=))))))

(setf
 (ningle:route *app* "/api/tags/:tag-id/folders")
 (labels ((run (tag-id)
            (let* ((tag
                    (mita.tag:store-get-tag *tag-store* tag-id))
                   (folder-namestrings
                    (mita.tag:tag-content-id-list *tag-store* tag "folder"))
                   (folders
                    (->> folder-namestrings
                         (mapcar (lambda (namestring)
                                   (mita.file:store-make-file
                                    *file-store* namestring)))
                         (remove-if-not #'mita.file:file-exists-p))))
              `(200 (:content-type "application/json")
                    (,(jsown:to-json
                       (mapcar (lambda (f)
                                 (folder-overview->jsown
                                  (folder->overview f *file-store*)))
                               folders)))))))
   (lambda (params)
     (run (cdr (assoc :tag-id params :test #'string=))))))

;;;

(defvar *static-root*
  (merge-pathnames
   "static/"
   (asdf:system-source-directory :mita-web)))

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

(defun warmup ()
  (bt:make-thread
   (lambda ()
     (ignore-errors
      (mita.file:store-prepare-cache *file-store*)))))
