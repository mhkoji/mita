(defpackage :mita.web.album
  (:use :cl)
  (:export :load-album-by-id
           :load-albums))
(in-package :mita.web.album)

(defun load-album-by-id (dep req album-id &key on-found on-not-found)
  (mita.db:with-connection (conn (mita.web.dep:get-db dep req))
    (let ((album (mita.album:load-album-by-id conn album-id)))
      (if album
          (funcall on-found album (mita.album:album-images conn album))
          (funcall on-not-found)))))
