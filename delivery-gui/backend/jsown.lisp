(defpackage :mita.gui.jsown
  (:use :cl))
(in-package :mita.gui.jsown)

(defvar *account-content-root* "/")

(defvar *account-thumbnail-root* "/")

(defmethod jsown:to-json ((state mita.gui.state:loading))
  (jsown:to-json
   (jsown:new-js
     ("type" "loading"))))
  
(defmethod jsown:to-json ((state mita.gui.state:viewing))
  (jsown:to-json
   (jsown:new-js
     ("type" "viewing")
     ("albums" (mita.gui.state:viewing-albums state))
     ("limit" (mita.gui.state:viewing-limit state))
     ("nextOffset" (or (mita.gui.state:viewing-next-offset state) :null))
     ("prevOffset" (or (mita.gui.state:viewing-prev-offset state) :null)))))

(defmethod jsown:to-json ((album mita.album:album))
  (jsown:to-json
   (jsown:new-js
     ("id"  (mita.album:album-id album))
     ("name" (mita.album:album-name album))
     ("thumbnail" (or (mita.album:album-thumbnail album) :null)))))

;; overwrite the definition in web
(defmethod jsown:to-json ((obj mita.image:image))
  (jsown:to-json
   (jsown:new-js
     ("id" (mita.image:image-id obj))
     ("path" (format nil "~A/~A"
                     (let ((src (mita.image:image-source obj)))
                       (cond ((eql src mita.image:+source-content+)
                              *account-content-root*)
                             ((eql src mita.image:+source-thumbnail+)
                              *account-thumbnail-root*)
                             (t
                              (assert nil))))
                     (mita.image:image-path obj))))))
