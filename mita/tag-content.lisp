(in-package :mita.tag)

(defmethod content-id ((album mita.album:album))
  (mita.album:album-id album))

(defmethod content-type ((album mita.album:album))
  :album)

(defmethod load-contents ((loader gateway)
                          (type (eql :album))
                          (content-id-list list))
  (mita.album::load-albums-in loader content-list))

(defmethod content-id ((page mita.page:page))
  (mita.page:page-id page))

(defmethod content-type ((page mita.page:page))
  :page)



