(defpackage :mita.load-albums
  (:use :cl)
  (:export :run))
(in-package :mita.load-albums)

(defun run (db offset limit &key on-loaded)
  (multiple-value-bind (albums full-loaded-p)
      (mita.db:with-connection (conn db)
        (mita.album:load-albums conn offset limit))
    (funcall on-loaded
             (if full-loaded-p (butlast albums) albums)
             (if (< 0 offset)
                 (max (- offset limit) 0)
                 nil)
             (if full-loaded-p
                 (+ offset limit)
                 nil))))
