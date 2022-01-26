(defpackage :mita.server.json
  (:use :cl)
  (:export :folder-detail
           :viewer
           :tag
           :tag-list
           :folder-overview-list
           :empty))
(in-package :mita.server.json)

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

(defun viewer->jsown (viewer)
  (jsown:new-js
    ("images" (mapcar #'file->jsown (mita.view:viewer-images viewer)))))

;;;

(defun folder-detail (x)
  (jsown:to-json (folder-detail->jsown x)))

(defun viewer (x)
  (jsown:to-json (viewer->jsown x)))

(defun tag (x)
  (jsown:to-json (tag->jsown x)))

(defun tag-list (xs)
  (jsown:to-json (mapcar #'tag->jsown xs)))

(defun folder-overview-list (xs)
  (jsown:to-json (mapcar #'folder-overview->jsown xs)))

(defun empty ()
  (jsown:to-json (jsown:new-js)))
