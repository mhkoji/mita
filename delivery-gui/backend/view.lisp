(defpackage :mita.gui.view
  (:use :cl)
  (:export :view
           :view-json
           :make-loading
           :make-viewing))
(in-package :mita.gui.view)

(defclass view ()
  ((json
    :initarg :json
    :reader view-json)))

(defun make-view (jsown)
  (make-instance 'view :json (jsown:to-json jsown)))

(defun make-loading ()
  (make-view
   (jsown:new-js
     ("type" "loading"))))

(defun make-viewing (&key albums has-prev has-next)
  (make-view
   (jsown:new-js
     ("type" "viewing")
     ("albums" albums)
     ("hasNext" (if has-next :t :f))
     ("hasPrev" (if has-prev :t :f)))))
