(defpackage :mita.gui.jsown
  (:use :cl))
(in-package :mita.gui.jsown)

(defvar *account-content-root* "/")

(defvar *account-thumbnail-root* "/")

(defmethod jsown:to-json ((album mita.album:album))
  (jsown:to-json
   (jsown:new-js
     ("id"  (mita.album:album-id album))
     ("name" (mita.album:album-name album))
     ("thumbnail" (or (mita.album:album-thumbnail album) :null)))))

;; overwrite the definition in web
(defmethod jsown:to-json ((obj mita.image:image))
  (let ((id (mita.image:image-id obj)))
    (jsown:to-json
     (jsown:new-js
       ("id" id)
       ("path" (format nil "~A/~A"
                       (let ((src (mita.image:image-source obj)))
                         (cond ((eql src mita.image:+source-content+)
                                *account-content-root*)
                               ((eql src mita.image:+source-thumbnail+)
                                *account-thumbnail-root*)
                               (t
                                (assert nil))))
                       (mita.image:image-path obj)))))))
