(in-package :mita.tag)

(defmethod content-id ((album mita.album:album))
  (mita.album:album-id album))

(defmethod content-type ((album mita.album:album))
  :album)

(defmethod content-thumbnail ((album mita.album:album))
  (mita.album:album-thumbnail album))

(defmethod content-name ((album mita.album:album))
  (mita.album:album-name album))

(defmethod load-contents ((loader mita.rdb:connection)
                          (type (eql :album))
                          (content-id-list list))
  (mita.album:load-albums-in loader content-id-list))
