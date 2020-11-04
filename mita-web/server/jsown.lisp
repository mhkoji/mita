(defpackage :mita.web.server.jsown
  (:use :cl)
  (:export :url-for
           :as-content))
(in-package :mita.web.server.jsown)

(defgeneric url-for (content))

(defmethod url-for ((c mita.album:album))
  (format nil "/albums/~A"
          (mita.id:to-string-short (mita.album:album-id c))))

(defmethod url-for ((c mita.page:page))
  (format nil "/pages/~A"
          (mita.id:to-string-short (mita.page:page-id c))))

(defmethod jsown:to-json ((obj mita.id:id))
  (jsown:to-json (mita.id:to-string-short obj)))

(defmethod jsown:to-json ((obj mita.image:image))
  (let* ((id (mita.image:image-id obj))
         (url (format nil "/images/~A" (mita.id:to-string-short id))))
    (jsown:to-json
     (jsown:new-js
       ("id" id)
       ("url" url)))))

(defmethod jsown:to-json ((obj mita.tag:tag))
  (jsown:to-json
   (jsown:new-js
     ("id" (mita.tag:tag-id obj))
     ("name" (mita.tag:tag-name obj)))))


(defun as-content (c)
  (jsown:new-js
    ("id"  (mita.tag:content-id c))
    ("url" (url-for c))
    ("type" (symbol-name (mita.tag:content-type c)))
    ("name" (or (mita.tag:content-name c) :null))
    ("thumbnail" (or (mita.tag:content-thumbnail c) :null))))