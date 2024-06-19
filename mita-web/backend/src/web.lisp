(defpackage :mita.web
  (:use :cl)
  (:export :service
           :service-file
           :service-folder
           :service-folder-images
           :service-folder-tags
           :service-folder-set-tags
           :service-list-tags
           :service-tag-add
           :service-tag-folders
           :service-warmup
           :make-service)
  (:import-from :mita.util.threading
                :->
                :->>))
(in-package :mita.web)

(defun file->view (file)
  (mita.web.view:make-file :path (mita.file:file-path file)))

(defun folder->overview (folder file-store)
  (mita.web.view:make-folder-overview
   :path (mita.file:file-path folder)
   :thumbnail-file
   (let ((file (first
                (->> (mita.file:folder-list-files folder file-store)
                     (remove-if #'mita.file:folder-p)))))
     (when file
       (file->view file)))))

(defun folder->detail (folder file-store)
  (let ((folder-files (mita.file:folder-list-files folder file-store)))
    (mita.web.view:make-folder-detail
     :path (mita.file:file-path folder)
     :file-list (->> (remove-if #'mita.file:folder-p folder-files)
                     (mapcar #'file->view))
     :folder-overview-list
     (let ((folders (remove-if-not #'mita.file:folder-p folder-files)))
       (->> (sort folders #'> :key #'mita.file:file-created-at)
            (mapcar (lambda (f)
                      (folder->overview f file-store))))))))

;;;

(defstruct service
  file-store
  (tag-store (mita.tag:make-store
              :dir *default-pathname-defaults*)))

;;;

(defun service-file (service namestring
                     &key on-found on-not-found)
  (let* ((file-store (service-file-store service))
         (file (mita.file:store-make-file file-store namestring))
         (full-path (mita.file:file-full-path file)))
    (if (uiop/filesystem:file-exists-p full-path)
        (funcall on-found full-path)
        (funcall on-not-found))))

(defun service-folder (service namestring
                       &key on-found on-not-found)
  (let* ((file-store (service-file-store service))
         (file (mita.file:store-make-file file-store namestring))
         (full-path (mita.file:file-full-path file)))
    (cond ((uiop/filesystem:directory-exists-p full-path)
           (assert (mita.file:folder-p file))
           (funcall on-found (folder->detail file file-store)))
          (t
           (funcall on-not-found)))))

(defun service-folder-images (service namestring
                              &key on-found on-not-found)
  ;; Assuming that all files in a folder are images.
  (let* ((file-store (service-file-store service))
         (file (mita.file:store-make-file file-store namestring))
         (full-path (mita.file:file-full-path file)))
    (cond ((uiop/filesystem:directory-exists-p full-path)
           (assert (mita.file:folder-p file))
           (let ((images (->> (mita.file:folder-list-files file file-store)
                              (remove-if #'mita.file:folder-p)
                              (mapcar #'file->view))))
             (funcall on-found images)))
          (t
           (funcall on-not-found)))))

(defun service-folder-tags (service namestring)
  (let ((folder (mita.file:store-make-file (service-file-store service)
                                           namestring)))
    (mita.tag:content-tags (service-tag-store service)
                           folder)))

(defun service-folder-set-tags (service namestring tag-id-list)
  (let ((folder (mita.file:store-make-file (service-file-store service)
                                           namestring)))
    (mita.tag:content-tags-set (service-tag-store service)
                               folder
                               tag-id-list)))

(defun service-list-tags (service)
  (mita.tag:store-list-tags (service-tag-store service)))

(defun service-tag-add (service name)
  (mita.tag:store-add-tag (service-tag-store service) name))

(defun service-tag-folders (service tag-id)
  (let ((file-store (service-file-store service))
        (tag (mita.tag:store-get-tag (service-tag-store service)
                                     tag-id)))
    (->> (mita.tag:tag-content-id-list (service-tag-store service)
                                       tag
                                       "folder")
         (mapcar (lambda (namestring)
                   (mita.file:store-make-file file-store namestring)))
         (remove-if-not #'mita.file:file-exists-p)
         (mapcar (lambda (folder)
                   (folder->overview folder file-store))))))

(defun service-warmup (service)
  (bt:make-thread
   (lambda ()
     (ignore-errors
      (mita.file:store-prepare-cache (service-file-store service))))))
