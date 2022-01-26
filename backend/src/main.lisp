(defpackage :mita.main
  (:use :cl)
  (:export :*file-store*
           :*tag-store*
           :show-path
           :show-viewer
           :folder-tags
           :folder-set-tags
           :list-tags
           :tag-add
           :tag-folders
           :warmup)
  (:import-from :mita.util.threading
                :->
                :->>))
(in-package :mita.main)

(defun file->view (file)
  (mita.view:make-file :path (mita.file:file-path file)))

(defun folder->overview (folder file-store)
  (mita.view:make-folder-overview
   :path (mita.file:file-path folder)
   :thumbnail-file
   (let ((file (first
                (->> (mita.file:folder-list-files folder file-store)
                     (remove-if #'mita.file:folder-p)))))
     (when file
       (file->view file)))))

(defun folder->detail (folder file-store)
  (let ((folder-files (mita.file:folder-list-files folder file-store)))
    (mita.view:make-folder-detail
     :path (mita.file:file-path folder)
     :file-list
     (->> (remove-if #'mita.file:folder-p folder-files)
          (mapcar #'file->view))
     :folder-overview-list
     (let ((folders (remove-if-not #'mita.file:folder-p folder-files)))
       (->> (sort folders #'> :key #'mita.file:file-created-at)
            (mapcar (lambda (f)
                      (folder->overview f file-store))))))))

;;;

(defvar *file-store*
  nil)

(defvar *tag-store*
  (mita.tag:make-store
   :dir *default-pathname-defaults*))

;;;

(defun show-path (namestring &key on-file on-folder on-not-found)
  (let ((file (mita.file:store-make-file *file-store* namestring)))
    (let ((full-path (mita.file:file-full-path file)))
      (cond ((uiop/filesystem:file-exists-p full-path)
             (funcall on-file full-path))
            ((uiop/filesystem:directory-exists-p full-path)
             (assert (mita.file:folder-p file))
             (funcall on-folder (folder->detail file *file-store*)))
            (t
             (funcall on-not-found))))))

(defun show-viewer (namestring &key on-found on-not-found)
  ;; Assuming that all files in a folder are images.
  (let ((file (mita.file:store-make-file *file-store* namestring)))
    (let ((full-path (mita.file:file-full-path file)))
      (cond ((uiop/filesystem:directory-exists-p full-path)
             (assert (mita.file:folder-p file))
             (let ((viewer (mita.view:make-viewer
                            :images
                            (->> (mita.file:folder-list-files
                                  file *file-store*)
                                 (remove-if #'mita.file:folder-p)
                                 (mapcar #'file->view)))))
               (funcall on-found viewer)))
            (t
             (funcall on-not-found))))))

(defun folder-tags (namestring)
  (let ((folder (mita.file:store-make-file *file-store* namestring)))
    (mita.tag:content-tags *tag-store* folder)))

(defun folder-set-tags (namestring tag-id-list)
  (let ((folder (mita.file:store-make-file *file-store* namestring)))
    (mita.tag:content-tags-set *tag-store* folder tag-id-list)))

(defun list-tags ()
  (mita.tag:store-list-tags *tag-store*))

(defun tag-add (name)
  (mita.tag:store-add-tag *tag-store* name))

(defun tag-folders (tag-id)
  (let ((tag (mita.tag:store-get-tag *tag-store* tag-id)))
    (->> (mita.tag:tag-content-id-list *tag-store* tag "folder")
         (mapcar (lambda (namestring)
                   (mita.file:store-make-file *file-store* namestring)))
         (remove-if-not #'mita.file:file-exists-p)
         (mapcar (lambda (folder)
                   (folder->overview folder *file-store*))))))

(defun warmup ()
  (bt:make-thread
   (lambda ()
     (ignore-errors
      (mita.file:store-prepare-cache *file-store*)))))
