(defpackage :mita.web.ningle
  (:use :cl)
  (:export :*file-store*
           :start))
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

(defun tag->jsown (tag)
  (jsown:new-js
    ("id" (format nil "~A" (mita.tag:tag-id tag)))
    ("name" (mita.tag:tag-name tag))))

(defun folder (namestring)
  (let ((file (mita.file:store-make-file *file-store* namestring)))
    (cond ((not (or (uiop/filesystem:file-exists-p
                     (mita.file:file-full-path file))
                    (uiop/filesystem:directory-exists-p
                     (mita.file:file-full-path file))))
           `(404 (:content-type "text/plain") ("Not found")))
          ((mita.file:folder-p file)
           `(200 (:content-type "text/html")
                 (,(mita.web.html:folder
                    (folder->detail file *file-store*)))))
          (t
           `(200 (:cache-control "max-age=31536000")
                 ,(mita.file:file-full-path file))))))

(defun view (namestring)
  (let ((file (mita.file:store-make-file *file-store* namestring)))
    `(200 (:content-type "text/html")
          (,(mita.web.html:view
             (when (mita.file:folder-p file)
               (mapcar #'file->view (remove-if #'mita.file:folder-p
                                               (mita.file:folder-list-files
                                                file *file-store*)))))))))

(defun folder-tags (namestring)
  (let* ((folder
          (mita.file:store-make-file *file-store* namestring))
         (tags
          (mita.tag:content-tags *tag-store* folder)))
    `(200 (:content-type "application/json")
          (,(jsown:to-json (mapcar #'tag->jsown tags))))))

(defun put-folder-tags (namestring tag-id-list)
  (let ((folder (mita.file:store-make-file *file-store* namestring)))
    (mita.tag:content-tags-set *tag-store* folder tag-id-list)
    `(200 (:content-type "application/json")
          (,(jsown:to-json (jsown:new-js))))))

(defun tags ()
  (let ((tags (mita.tag:store-list-tags *tag-store*)))
    `(200 (:content-type "application/json")
          (,(jsown:to-json (mapcar #'tag->jsown tags))))))

(defun put-tag (name)
  (let ((tag (mita.tag:store-add-tag *tag-store* name)))
    `(200 (:content-type "application/json")
          (,(jsown:to-json (tag->jsown tag))))))

(defun tag-folders (tag-id)
  (let* ((tag
          (mita.tag:store-get-tag *tag-store* tag-id))
         (folder-namestrings
          (mita.tag:tag-content-id-list *tag-store* tag "folder"))
         (folders
           (mapcar (lambda (namestring)
                     (mita.file:store-make-file *file-store* namestring))
                   folder-namestrings)))
    `(200 (:content-type "application/json")
          (,(jsown:to-json
             (mapcar (lambda (f)
                       (mita.web.html::folder-overview->jsown
                        (folder->overview f *file-store*)))
                     folders))))))

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
  (let ((app (make-instance 'ningle:app)))
    (setf (ningle:route app "/")
          (lambda (params)
            (declare (ignore params))
            '(302 (:location "/folder/") nil)))
    (setf (ningle:route app "/folder*")
          (lambda (params)
            (folder (cadr (assoc :splat params)))))
    (setf (ningle:route app "/view*")
          (lambda (params)
            (view (cadr (assoc :splat params)))))
    (setf (ningle:route app "/api/tags")
          (lambda (params)
            (declare (ignore params))
            (tags)))
    (setf (ningle:route app "/api/folder/tags")
          (lambda (params)
            (folder-tags
             (cdr (assoc "path" params :test #'string=)))))
    (setf (ningle:route app "/api/folder/tags"
                        :method :put)
          (lambda (params)
            (put-folder-tags
             (cdr (assoc "path" params :test #'string=))
             (mapcar #'uuid:make-uuid-from-string
                     (cdr (assoc "tag-id-list" params :test #'string=))))))
    (setf (ningle:route app "/api/tags/_create"
                        :method :post)
          (lambda (params)
            (put-tag (cdr (assoc "name" params :test #'string=)))))
    (setf (ningle:route app "/api/tags/:tag-id/folders")
          (lambda (params)
            (tag-folders
             (uuid:make-uuid-from-string
              (cdr (assoc :tag-id params :test #'string=))))))
    (setq *handler*
          (clack:clackup
           (lack:builder
            (:static :path "/static/" :root *static-root*)

            #+nil
            (lambda (next)
              (lambda (env)
                (log:info env)
                (funcall next env)))

            app)
           :address "0.0.0.0"
           :use-thread use-thread
           :port port))))
