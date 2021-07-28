(defpackage :mita.gui.view
  (:use :cl)
  (:export :view
           :view-json
           :make-loading
           :make-album-list
           :make-album))
(in-package :mita.gui.view)

(defclass view ()
  ((jsown
    :initarg :jsown
    :reader view-jsown)))

(defun make-view (jsown)
  (make-instance 'view :jsown jsown))

(defun view-json (state-name view)
  (jsown:to-json
   (jsown:extend-js (view-jsown view)
     ("state" state-name))))

(defun make-loading ()
  (make-view
   (jsown:new-js
     ("type" "loading"))))

(defun make-album-list (&key albums has-prev has-next)
  (make-view
   (jsown:new-js
     ("type" "album-list")
     ("albums" albums)
     ("hasNext" (if has-next :t :f))
     ("hasPrev" (if has-prev :t :f)))))

(defun make-album (&key name images)
  (make-view
   (jsown:new-js
     ("type" "album")
     ("name" name)
     ("images" images))))
