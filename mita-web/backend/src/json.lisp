(defpackage :mita.web.json
  (:use :cl)
  (:export :folder-detail
           :viewer
           :tag
           :tag-list
           :folder-overview-list
           :empty))
(in-package :mita.web.json)

(defun file->obj (file)
  (alexandria:plist-hash-table
   (list "path" (namestring (mita.web.view:file-path file))
         "url"  (format nil "/file~A" (mita.web.view:file-path file)))
   :test #'equal))

(defun folder-overview->obj (overview)
  (let ((path (mita.web.view:folder-overview-path overview)))
    (alexandria:plist-hash-table
     (list "path" (namestring path)
           "url" (format nil "/folder~A" path)
           "thumbnail"
           (let ((file (mita.web.view:folder-overview-thumbnail-file
                        overview)))
             (if file (file->obj file) nil)))
     :test #'equal)))

(defun folder-detail->obj (detail)
  (alexandria:plist-hash-table
   (list "path"
         (namestring (mita.web.view:folder-detail-path detail))
         "files"
         (or (mapcar #'file->obj
                     (mita.web.view:folder-detail-file-list detail))
             #())
         "folders"
         (or (mapcar #'folder-overview->obj
                     (mita.web.view:folder-detail-folder-overview-list detail))
             #()))
   :test #'equal))

(defun tag->obj (tag)
  (alexandria:plist-hash-table
   (list "id" (format nil "~A" (mita.tag:tag-id tag))
         "name" (mita.tag:tag-name tag))
   :test #'equal))

;;;

(defun folder-detail (x)
  (with-output-to-string (stream)
    (yason:with-output (stream)
      (yason:encode (folder-detail->obj x)
                    stream))))

(defun viewer (images)
  (with-output-to-string (stream)
    (yason:with-output (stream)
      (yason:encode
       (alexandria:plist-hash-table
        (list "images" (or (mapcar #'file->obj images)
                           #()))
        :test #'equal)
       stream))))

(defun tag (x)
  (with-output-to-string (stream)
    (yason:with-output (stream)
      (yason:encode (tag->obj x) stream))))

(defun tag-list (xs)
  (with-output-to-string (stream)
    (yason:with-output (stream)
      (yason:encode (or (mapcar #'tag->obj xs)
                        #())
                    stream))))

(defun folder-overview-list (xs)
  (with-output-to-string (stream)
    (yason:with-output (stream)
      (yason:encode (or (mapcar #'folder-overview->obj xs)
                        #())
                    stream))))

(defun empty ()
  (with-output-to-string (stream)
    (yason:with-output (stream)
      (yason:encode (make-hash-table :test #'equal)
                    stream))))
