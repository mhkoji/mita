(in-package :mita.rdb)

(defmethod mita.image:save-images ((conn connection)
                                   (images list))
  (image-insert conn images)
  (values))

(defmethod mita.image:load-images-by-ids ((conn connection)
                                          (image-ids list))
  (image-select-by-ids conn image-ids))

(defmethod mita.image:delete-images ((conn connection)
                                     (image-ids list))
  (image-delete conn image-ids))
