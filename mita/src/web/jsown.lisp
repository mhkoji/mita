(defpackage :mita.web.jsown
  (:use :cl)
  (:export :url-for
           :as-content
           :as-image))
(in-package :mita.web.jsown)

(defgeneric as-image (obj))

(defmethod as-image ((obj mita.image:image))
  (let* ((id (mita.image:image-id obj))
         (url (format nil "/images/~A" (mita.id:to-string-short id))))
    (jsown:new-js
      ("id" id)
      ("url" url))))

(defmethod as-image ((obj mita.file:file))
  (let ((path (mita.file:file-path obj)))
    (jsown:new-js
      ("id" path)
      ("url" (format nil "/dir~A" path)))))


(defgeneric url-for (content))

(defmethod url-for ((c mita.album:album))
  (format nil "/albums/~A"
          (mita.id:to-string-short (mita.album:album-id c))))

(defmethod url-for ((c mita.file:file))
  (format nil "/dir~A" (mita.file:file-path c)))


(defmethod jsown:to-json ((obj mita.id:id))
  (jsown:to-json (mita.id:to-string-short obj)))

(defmethod jsown:to-json ((obj mita.image:image))
  (jsown:to-json (as-image obj)))

(defmethod jsown:to-json ((obj mita.tag:tag))
  (jsown:to-json
   (jsown:new-js
     ("id" (mita.tag:tag-id obj))
     ("name" (mita.tag:tag-name obj)))))

(defmethod jsown:to-json ((obj mita.file:file))
  (jsown:to-json
   (jsown:new-js
     ("url" (url-for obj))
     ("name" (mita.file:file-name obj))
     ("path" (mita.file:file-path obj))
     ("isDirectory" (mita.file:folder-p obj))
     ("size" (mita.file:file-size obj)))))

(defun as-content (c)
  (jsown:new-js
    ("id"  (mita.tag:content-id c))
    ("url" (url-for c))
    ("type" (symbol-name (mita.tag:content-type c)))
    ("name" (or (mita.tag:content-name c) :null))
    ("thumbnail" (or (mita.tag:content-thumbnail c) :null))))
