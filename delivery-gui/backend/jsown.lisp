(defpackage :mita.gui.jsown
  (:use :cl))
(in-package :mita.gui.jsown)

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
