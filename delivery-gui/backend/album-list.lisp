(defpackage :mita.gui.album-list
  (:use :cl)
  (:export :update-state
           :loading
           :listed
           :listed-albums
           :listed-limit
           :listed-next-offset
           :listed-prev-offset
           :list-albums
           :next-albums
           :prev-albums))
(in-package :mita.gui.album-list)

(defgeneric update-state (gateway state))

(defstruct (loading (:include mita.gui.state:loading)))

(defstruct listed
  albums
  limit
  prev-offset
  next-offset)

(defun list-albums (offset limit db gw)
  (update-state gw (make-loading))

  (mita.db:with-connection (conn db)
    (multiple-value-bind (albums full-loaded-p)
        (mita.album:load-albums conn offset limit)
      (update-state gw (make-listed
                        :albums albums
                        :limit limit
                        :prev-offset (when (< 0 offset)
                                       (max (- offset limit) 0))
                        :next-offset (when full-loaded-p
                                       (+ offset limit)))))))

(defun next-albums (listed db gw)
  (list-albums (listed-next-offset listed) (listed-limit listed) db gw))

(defun prev-albums (listed db gw)
  (list-albums (listed-prev-offset listed) (listed-limit listed) db gw))
