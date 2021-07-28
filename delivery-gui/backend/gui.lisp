(defpackage :mita.gui
  (:use :cl)
  (:export :update-view
           :update-state
           :list-albums
           :select-album))
(in-package :mita.gui)

(defgeneric update-state (gateway state))

(defgeneric update-view (gateway view))

(defun list-albums (db offset limit gw)
  (update-state gw (mita.gui.state:make-album-list))
  (update-view gw (mita.gui.view:make-loading))

  (mita.db:with-connection (conn db)
    (multiple-value-bind (albums full-loaded-p)
        (mita.album:load-albums conn offset limit)

      (let ((state (mita.gui.state:make-album-list
                    :albums albums
                    :limit limit
                    :prev-offset (when (< 0 offset)
                                   (max (- offset limit) 0))
                    :next-offset (when full-loaded-p
                                   (+ offset limit)))))
        (update-state gw state)
        (update-view gw (mita.gui.view:make-album-list
                         :albums
                         (mita.gui.state:album-list-albums state)
                         :has-prev
                         (mita.gui.state:album-list-prev-offset state)
                         :has-next
                         (mita.gui.state:album-list-next-offset state)))))))

(defun select-album (db album-id gw)
  (update-state gw (mita.gui.state:make-album))
  (update-view gw (mita.gui.view:make-loading))

  (mita.db:with-connection (conn db)
    (let ((album (mita.album:load-album-by-id conn album-id)))
      (let ((images (mita.album:album-images conn album)))
        (let ((state (mita.gui.state:make-album :images images)))
          (update-state gw state)
          (update-view gw (mita.gui.view:make-album
                           :name (mita.album:album-name album)
                           :images images)))))))
