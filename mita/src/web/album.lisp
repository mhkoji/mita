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

(defun load-albums (dep req q offset limit &key on-loaded)
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
                   (jsown:val json "input"))))
    (let ((db (mita.web.dep:get-db dep req)))
      (if (not q)
          (mita.load-albums:run db offset limit :on-loaded on-loaded)
          (let* ((q-json
                  (jsown:parse q))
                 (sort
                  (when (jsown:keyp q-json "sort")
                    (json->sort (jsown:val q-json "sort"))))
                 (condition
                  (when (jsown:keyp q-json "condition")
                    (let ((items (mapcar #'json->condition-item
                                         (jsown:val q-json "condition"))))
                      (when items
                        (labels ((rec (node rest)
                                   (if (null rest)
                                       node
                                       (rec `(:and ,node ,(car rest))
                                            (cdr rest)))))
                          (rec (car items) (cdr items)))))))
                 (query
                  (make-instance 'mita.album.list:query
                                 :condition condition
                                 :sort sort)))
            (mita.load-albums:run/query
             db query offset limit :on-loaded on-loaded))))))
