(defpackage :mita.gui
  (:use :cl)
  (:export :update-state
           :view-albums))
(in-package :mita.gui)

(defgeneric update-state (gateway state))

(defun view-albums (db offset limit gw)
  (update-state gw (mita.gui.state:make-loading))

  (mita.db:with-connection (conn db)
    (multiple-value-bind (albums full-loaded-p)
        (mita.album:load-albums conn offset limit)
      (update-state gw (mita.gui.state:make-viewing
                        :albums albums
                        :limit limit
                        :prev-offset (when (< 0 offset)
                                       (max (- offset limit) 0))
                        :next-offset (when full-loaded-p
                                       (+ offset limit)))))))
