(defpackage :mita.web.json
  (:use :cl)
  (:export :folder-detail
           :viewer
           :tag
           :tag-list
           :folder-overview-list
           :empty))
(in-package :mita.web.json)

(defun file->jsown (file)
  (jsown:new-js
    ("path" (namestring (mita.web.view:file-path file)))
    ("url"  (format nil "/folder~A" (mita.web.view:file-path file)))))

(defun folder-overview->jsown (overview)
  (let ((path (mita.web.view:folder-overview-path overview)))
    (jsown:new-js
      ("path" (namestring path))
      ("url" (format nil "/folder~A" path))
      ("thumbnail"
       (let ((file (mita.web.view:folder-overview-thumbnail-file
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

(defun folder-detail (x)
  (jsown:to-json (folder-detail->jsown x)))

(defun viewer (images)
  (jsown:to-json
   (jsown:new-js
     ("images" (mapcar #'file->jsown images)))))

(defun tag (x)
  (jsown:to-json (tag->jsown x)))

(defun tag-list (xs)
  (jsown:to-json (mapcar #'tag->jsown xs)))

(defun folder-overview-list (xs)
  (jsown:to-json (mapcar #'folder-overview->jsown xs)))

(defun empty ()
  (jsown:to-json (jsown:new-js)))
