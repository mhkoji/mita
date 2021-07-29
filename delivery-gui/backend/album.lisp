(defpackage :mita.gui.album
  (:use :cl)
  (:export :update-state
           :loading
           :album
           :album-name
           :album-images
           :list-images))
(in-package :mita.gui.album)

(defgeneric update-state (gateway state))

(defstruct (loading (:include mita.gui.state:loading)))

(defstruct album
  name
  images)

(defun list-images (album-id db gw)
  (update-state gw (make-loading))

  (mita.db:with-connection (conn db)
    (let ((album (mita.album:load-album-by-id conn album-id)))
      (update-state gw (make-album
                        :name (mita.album:album-name album)
                        :images (mita.album:album-images conn album))))))
