(defpackage :mita.load-albums
  (:use :cl)
  (:export :run
           :run/query))
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

(defun run/query (db q-string offset limit &key on-loaded)
  (labels ((json->sort (json)
             (mita.album.list:make-sort
              (alexandria:make-keyword
               (string-upcase (jsown:val json "field")))
              (alexandria:make-keyword
               (string-upcase (jsown:val json "direction")))))
           (json->condition-item (json)
             (list (alexandria:make-keyword
                    (string-upcase (jsown:val json "op")))
                   (alexandria:make-keyword
                    (string-upcase (jsown:val json "field")))
                   (jsown:val json "input")))
           (json->query (json)
             (let ((sort
                    (when (jsown:keyp json "sort")
                      (json->sort (jsown:val json "sort"))))
                   (condition
                    (when (jsown:keyp json "condition")
                      (let ((items (mapcar #'json->condition-item
                                           (jsown:val json "condition"))))
                        (when items
                          (labels ((rec (node rest)
                                     (if (null rest)
                                         node
                                         (rec `(:and ,node ,(car rest))
                                              (cdr rest)))))
                            (rec (car items) (cdr items))))))))
               (make-instance 'mita.album.list:query
                              :condition condition :sort sort))))
    (let ((query (json->query (jsown:parse q-string))))
      (multiple-value-bind (albums has-more)
          (mita.db:with-connection (conn db)
            (mita.album.list:run conn query offset limit))
        (funcall on-loaded
                 albums
                 (if (< 0 offset)
                     (max (- offset limit) 0)
                     nil)
                 (if has-more
                     (+ offset limit)
                     nil))))))
