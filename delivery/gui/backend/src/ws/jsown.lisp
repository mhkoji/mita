(defpackage :mita.gui.ws.jsown
  (:use :cl))
(in-package :mita.gui.ws.jsown)

(defvar *content-root* "/")

(defvar *thumbnail-root* "/")

(defmethod jsown:to-json ((album mita.album:album))
  (jsown:to-json
   (jsown:new-js
     ("id"  (mita.album:album-id album))
     ("name" (mita.album:album-name album))
     ("thumbnail" (or (mita.album:album-thumbnail album) :null)))))

(defmethod jsown:to-json ((obj mita.id:id))
  (jsown:to-json (mita.id:to-string-short obj)))

(defmethod jsown:to-json ((obj mita.tag:tag))
  (jsown:to-json
   (jsown:new-js
     ("id" (mita.tag:tag-id obj))
     ("name" (mita.tag:tag-name obj)))))

(defmethod jsown:to-json ((obj mita.image:image))
  (let ((id (mita.image:image-id obj)))
    (jsown:to-json
     (jsown:new-js
       ("id" id)
       ("path" (format nil "~A/~A"
                       (let ((src (mita.image:image-source obj)))
                         (cond ((eql src mita.image:+source-content+)
                                *content-root*)
                               ((eql src mita.image:+source-thumbnail+)
                                *thumbnail-root*)
                               (t
                                (assert nil))))
                       (mita.image:image-path obj)))))))
