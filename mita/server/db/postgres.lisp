(defpackage :mita.db.postgres
  (:use :cl)
  (:export :with-db
           :create-admin-database
           :create-database
           :drop-database
           :postgres))
(in-package :mita.db.postgres)

(defclass postgres (mita.db.relational:rdb
                    mita.util.postgres:postgres)
  ())

(defmacro with-db ((db db-name connector) &body body)
  (let ((g (gensym)))
    `(mita.util.postgres:with-db (,g ,db-name ,connector)
       (let ((,db (change-class ,g 'postgres)))
         ,@body))))

;;;

(defun create-admin-database (postgres-dir db-name connector)
  (with-db (db db-name connector)
    (declare (ignore db))
    (postmodern:execute-file
     (merge-pathnames postgres-dir "./admin-ddl.sql"))))

;;;

(defun create-database (postgres-dir admin-db-name db-name connector)
  (postmodern:with-connection
      (mita.util.postgres::connector->spec admin-db-name connector)
    (postmodern:query
     (format nil "CREATE DATABASE ~A" db-name)))
  (postmodern:with-connection
      (mita.util.postgres::connector->spec db-name connector)
    (postmodern:execute-file
     (merge-pathnames postgres-dir "./mita-ddl.sql"))))

(defun drop-database (admin-db-name db-name connector)
  (postmodern:with-connection
      (mita.util.postgres::connector->spec admin-db-name connector)
    (postmodern:query
     (format nil "DROP DATABASE IF EXISTS ~A" db-name))))

;;;

(defmethod mita.db:page-text-update ((db postgres)
                                     (page-id mita.id:id)
                                     (text string))
  (mita.util.postgres:execute db
   "UPDATE page_text set string = $1 where page_id = $2"
   (list text (mita.id:to-string page-id))))

(defmethod mita.db:album-select-album-ids ((db postgres) offset limit)
  (mapcar (lambda (row)
            (mita.id:parse (car row)))
          (mita.util.postgres:execute db
           (concatenate 'string
            "SELECT album_id FROM albums"
            " ORDER BY created_on DESC OFFSET $1 LIMIT $2")
           (list offset limit))))

(defmethod mita.db:tag-update ((db postgres)
                               (tag-id mita.id:id)
                               (name string))
  (mita.util.postgres:execute db
   "UPDATE tags SET name = $1 where tag_id = $2"
   (list name (mita.id:to-string tag-id))))

;;;

(defmethod mita.db.relational:insert-into ((db postgres)
                                           table-name
                                           column-name-list
                                           values-list)
  (mita.util.postgres:execute db
   (with-output-to-string (s)
     (format s "INSERT INTO ~A" table-name)
     (format s " (~{~A~^,~})" column-name-list)
     (format s " VALUES ~{~A~^,~}"
             (let ((i 0)
                   (column-count (length column-name-list)))
               (loop repeat (length values-list)
                     for vals = (loop repeat column-count
                                      collect (format nil "$~A" (incf i)))
                     collect (format nil "(~{~A~^,~})" vals)))))
   (reduce #'append values-list)))


(defun parse-clause (clause)
  (let ((i 0))
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
                          (loop repeat (length values)
                                collect (format nil "$~A" (incf i))))
                         values)))
                     (:where
                      (rec (second clause)
                       (lambda (cond acc-values)
                         (funcall k
                          (format nil "WHERE ~A" cond)
                          acc-values))))))))
      (rec clause #'list))))

(defmethod mita.db.relational:delete-from ((db postgres) table-name
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
      (mita.util.postgres:execute db query-string args))))

(defmethod mita.db.relational:select-from ((db postgres)
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
      (mita.util.postgres:execute db query-string args))))
