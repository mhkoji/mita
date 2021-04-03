(defpackage :mita.db.mysql
  (:use :cl)
  (:export :with-db
           :create-admin-database
           :create-database
           :drop-database
           :mysql))
(in-package :mita.db.mysql)

(defclass mysql (mita.db.relational:rdb
                 mita.util.mysql:mysql)
  ())

(defmacro with-db ((db db-name connector) &body body)
  (let ((g (gensym)))
    `(mita.util.mysql:with-db (,g ,db-name ,connector)
       (let ((,db (change-class ,g 'mysql)))
         ,@body))))

;;;

(defun create-admin-database (mysql-dir db-name connector)
  (with-db (db db-name connector)
    (dolist (sql (cl-ppcre:split
                  (format nil "~%~%")
                  (alexandria:read-file-into-string
                   (merge-pathnames mysql-dir "./admin-ddl.sql"))))
      (cl-dbi:do-sql (mita.util.mysql::mysql-conn db) sql))))

;;;

(defun create-database (mysql-dir admin-db-name db-name connector)
  (declare (ignore admin-db-name))
  (with-db (db nil connector)
    (cl-dbi:do-sql (mita.util.mysql::mysql-conn db)
      (format nil "CREATE DATABASE IF NOT EXISTS ~A" db-name)))
  (with-db (db db-name connector)
    (dolist (sql (cl-ppcre:split
                  (format nil "~%~%")
                  (alexandria:read-file-into-string
                   (merge-pathnames mysql-dir "./mita-ddl.sql"))))
      (cl-dbi:do-sql (mita.util.mysql::mysql-conn db) sql))))

(defun drop-database (admin-db-name db-name connector)
  (declare (ignore admin-db-name))
  (with-db (db nil connector)
    (cl-dbi:do-sql (mita.util.mysql::mysql-conn db)
      (format nil "DROP DATABASE IF EXISTS ~A" db-name))))

;;;

(defmethod mita.db:page-text-update ((db mysql)
                                     (page-id mita.id:id)
                                     (text string))
  (mita.util.mysql:execute db
   "UPDATE page_text set string = ? where page_id = ?"
   (list text (mita.id:to-string page-id))))

(defmethod mita.db:album-select-album-ids ((db mysql) offset limit)
  (mapcar (lambda (plist)
            (let ((row (mapcar #'cdr (alexandria:plist-alist plist))))
              (mita.id:parse (car row))))
          (mita.util.mysql:execute db
           (concatenate 'string
            "SELECT album_id FROM albums"
            " ORDER BY created_on DESC LIMIT ?, ?")
           (list offset limit))))

(defmethod mita.db:tag-update ((db mysql)
                               (tag-id mita.id:id)
                               (name string))
  (mita.util.mysql:execute db
   "UPDATE tags SET name = ? where tag_id = ?"
   (list name (mita.id:to-string tag-id))))

;;;

(defmethod mita.db.relational:insert-into ((db mysql)
                                           table-name
                                           column-name-list
                                           values-list)
  (mita.util.mysql:execute db
   (with-output-to-string (s)
     (format s "INSERT INTO ~A" table-name)
     (format s " (~{~A~^,~})" column-name-list)
     (format s " VALUES ~{~A~^,~}"
             (let ((column-count (length column-name-list)))
               (loop repeat (length values-list)
                     for vals = (make-list column-count
                                           :initial-element "?")
                     collect (format nil "(~{~A~^,~})" vals)))))
   (reduce #'append values-list)))

(defun parse-clause (clause)
  (labels ((rec (clause k)
             (if (not (keywordp (car clause)))
                 (funcall k
                  (if (null clause)
                      ""
                      (format nil "~A" clause))
                  nil)
                 (ecase (car clause)
                   (:and
                    (destructuring-bind (left right) (cdr clause)
                      (rec left
                       (lambda (l-cond l-acc-values)
                         (rec right
                          (lambda (r-cond r-acc-values)
                            (funcall k
                             (format nil "~A AND ~A" l-cond r-cond)
                             (append l-acc-values r-acc-values))))))))
                   ((:in :=)
                    (let ((op (car clause))
                          (column-name (second clause)))
                      (rec (third clause)
                       (lambda (cond acc-values)
                         (funcall k
                          (format nil "(~A ~A ~A)" column-name op cond)
                          acc-values)))))
                   (:p
                    (let ((values (alexandria:ensure-list
                                   (second clause))))
                      (funcall k
                       (format nil "(~{~A~^,~})"
                        (make-list (length values) :initial-element "?"))
                       values)))
                   (:where
                    (rec (second clause)
                     (lambda (cond acc-values)
                       (funcall k
                        (format nil "WHERE ~A" cond)
                        acc-values))))))))
    (rec clause #'list)))

(defmethod mita.db.relational:delete-from ((db mysql) table-name
                                           &key where)
  (let ((args nil))
    (let ((query-string
           (with-output-to-string (s)
             (format s "DELETE FROM ~A" table-name)
             (when where
               (destructuring-bind (cond-string values)
                   (parse-clause (list :where where))
                 (format s " ~A" cond-string)
                 (alexandria:appendf args values))))))
      (mita.util.mysql:execute db query-string args))))

(defmethod mita.db.relational:select-from ((db mysql)
                                           column-names
                                           table-name
                                           &key where
                                                order-by)
  (let ((args nil))
    (let ((query-string
           (with-output-to-string (s)
             (format s "SELECT ~A FROM ~A" column-names table-name)
             (when where
               (destructuring-bind (cond-string vals)
                   (parse-clause (list :where where))
                 (format s " ~A" cond-string)
                 (alexandria:appendf args vals)))
             (when order-by
               (format s " ORDER BY ~A" order-by)))))
      (mapcar (lambda (plist)
                (mapcar #'cdr (alexandria:plist-alist plist)))
              (mita.util.mysql:execute db query-string args)))))
