(defpackage :mita.web.folder
  (:use :cl)
  (:export :file
           :file-path
           :file-full-path
           :overview
           :overview-path
           :overview-thumbnail-file
           :detail
           :detail-path
           :detail-file-list
           :detail-overview-list
           :service
           :service-get-tags
           :service-set-tags
           :service-tag-namestring-list
           :service-file-store
           :service-path-content
           :service-folder-images
           :service-folder-tags
           :service-folder-set-tags
           :service-tag-folders)
  (:import-from :mita.util.threading
                :->
                :->>))
(in-package :mita.web.folder)

(defstruct file
  path)

(defstruct overview
  path
  thumbnail-file)

(defstruct detail
  path
  file-list
  overview-list)

(defun file->view (file)
  (make-file :path (mita.file:file-path file)))

(defun folder->overview (folder file-store)
  (make-overview
   :path
   (mita.file:file-path folder)
   :thumbnail-file
   (let ((folder-files
          (->> (mita.file:folder-list-files folder file-store)
               (remove-if #'mita.file:folder-p))))
     (when folder-files
       (file->view (first folder-files))))))

(defun folder->detail (folder file-store)
  (let ((folder-files (mita.file:folder-list-files folder file-store)))
    (make-detail
     :path
     (mita.file:file-path folder)
     :file-list
     (->> (remove-if #'mita.file:folder-p folder-files)
          (mapcar #'file->view))
     :overview-list
     (let ((folders (remove-if-not #'mita.file:folder-p folder-files)))
       (->> (sort folders #'> :key #'mita.file:file-created-at)
            (mapcar (lambda (f)
                      (folder->overview f file-store))))))))

;;;

(defgeneric service-file-store (service))
(defgeneric service-get-tags (service folder))
(defgeneric service-set-tags (service folder tag-id-set))
(defgeneric service-tag-namestring-list (service tag-id))

(defun service-path-content (service namestring
                             &key on-file on-folder on-not-found)
  (let* ((file-store (service-file-store service))
         (file (mita.file:store-make-file file-store namestring))
         (full-path (mita.file:file-full-path file)))
    (cond ((uiop/filesystem:file-exists-p full-path)
           (funcall on-file full-path))
          ((uiop/filesystem:directory-exists-p full-path)
           (assert (mita.file:folder-p file))
           (funcall on-folder (folder->detail file file-store)))
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

;;;

(defun service-folder-tags (service namestring)
  (let ((folder (mita.file:store-make-file (service-file-store service)
                                           namestring)))
    (service-get-tags service folder)))

(defun service-folder-set-tags (service namestring tag-id-list)
  (let ((folder (mita.file:store-make-file (service-file-store service)
                                           namestring)))
    (service-set-tags service folder tag-id-list)))

(defun service-tag-folders (service tag-id)
  (let ((namestring-list (service-tag-namestring-list service tag-id))
        (file-store (service-file-store service)))
    (->> namestring-list
         (mapcar (lambda (namestring)
                   (mita.file:store-make-file file-store namestring)))
         (remove-if-not #'mita.file:file-exists-p)
         (mapcar (lambda (folder)
                   (folder->overview folder file-store))))))
