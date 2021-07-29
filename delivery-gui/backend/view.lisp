(defpackage :mita.gui.view
  (:use :cl)
  (:export :to-json))
(in-package :mita.gui.view)

(defgeneric make-view (state))

(defmethod make-view (state)
  (jsown:new-js))

(defun to-json (state-category state)
  (jsown:to-json
   (jsown:new-js
     ("name" (string-downcase (symbol-name state-category)))
     ("view" (make-view state)))))

(defun json-bool (val)
  (if val :t :f))

(defmethod make-view ((state mita.gui.state:loading))
  (jsown:new-js
    ("type" "loading")))

(defmethod make-view ((state mita.gui.album-list:listed))
  (jsown:new-js
    ("type"
     "album-list")
    ("albums"
     (mita.gui.album-list:listed-albums state))
    ("hasNext"
     (json-bool (mita.gui.album-list:listed-next-offset state)))
    ("hasPrev"
     (json-bool (mita.gui.album-list:listed-prev-offset state)))))

(defmethod make-view ((state mita.gui.album:album))
  (jsown:new-js
    ("type"
     "album")
    ("name"
     (mita.gui.album:album-name state))
    ("images"
     (mita.gui.album:album-images state))))

(defmethod make-view ((state mita.gui.tag-edit:editing))
  (jsown:new-js
    ("type"
     "editing")
    ("isTagAdded"
     (json-bool
      (mita.gui.tag-edit:editing-is-tag-added state)))
    ("tags"
     (mita.gui.tag-edit:editing-tags state))
    ("contentTags"
     (mita.gui.tag-edit:editing-content-tags state))))

(defmethod make-view ((state mita.gui.tag-edit:saving))
  (jsown:new-js
    ("type"
     "saving")))

(defmethod make-view ((state mita.gui.tag-edit:saved))
  (jsown:new-js
    ("type"
     "saved")))
