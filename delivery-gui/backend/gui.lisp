(defpackage :mita.gui
  (:use :cl)
  (:export :update-view
           :update-state
           :view-albums))
(in-package :mita.gui)

(defgeneric update-state (gateway state))

(defgeneric update-view (gateway view))

(defun view-albums (db offset limit gw)
  (update-view gw (mita.gui.view:make-loading))

  (mita.db:with-connection (conn db)
    (multiple-value-bind (albums full-loaded-p)
        (mita.album:load-albums conn offset limit)

      (let ((state (mita.gui.state:make-viewing
                    :albums albums
                    :limit limit
                    :prev-offset (when (< 0 offset)
                                   (max (- offset limit) 0))
                    :next-offset (when full-loaded-p
                                   (+ offset limit)))))
        (update-state gw state)
        (update-view gw (mita.gui.view:make-viewing
                         :albums
                         (mita.gui.state:viewing-albums state)
                         :has-prev
                         (mita.gui.state:viewing-prev-offset state)
                         :has-next
                         (mita.gui.state:viewing-next-offset state)))))))
