(in-package :mita.tag)

(defmethod content-id ((album mita.album:album))
  (mita.album:album-id album))

(defmethod content-type ((album mita.album:album))
  :album)

(defmethod content-thumbnail ((album mita.album:album))
  (mita.album:album-thumbnail album))

(defmethod content-name ((album mita.album:album))
  (mita.album:album-name album))

(defmethod load-contents ((loader mita.db:connection)
                          (type (eql :album))
                          (content-id-list list))
  (mita.album::load-albums-in loader content-id-list))

(defmethod content-id ((page mita.page:page))
  (mita.page:page-id page))

(defmethod content-type ((page mita.page:page))
  :page)

(defmethod content-thumbnail ((page mita.page:page))
  nil)

(defmethod content-name ((page mita.page:page))
  nil)

(defmethod load-contents ((loader mita.db:connection)
                          (type (eql :page))
                          (content-id-list list))
  ;; TODO: too many db accesses
  (mapcar (lambda (id)
            (mita.page:load-page-by-id loader id))
          content-id-list))
