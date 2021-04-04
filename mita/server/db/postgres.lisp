(defpackage :mita.db.postgres
  (:use :cl)
  (:export :make-locator
           :postgres
           :connection
           :create-admin-database
           :create-database
           :drop-database))
(in-package :mita.db.postgres)

(defstruct locator user host port)

(defun make-spec (db-name locator)
  (list db-name
        (locator-user locator)
        "" ;; password
        (locator-host locator)
        :port (locator-port locator)
        :pooled-p t))

(defclass postgres (mita.db:db)
  ((db-name
    :initarg :db-name
    :reader postgres-db-name)
   (locator
    :initarg :locator
    :reader postgres-locator)))

(defclass connection (mita.db.relational:connection)
  ((impl
    :initarg :impl
    :reader connection-impl)))

(defmethod mita.db:call-with-connection ((db postgres) fn)
  (postmodern:with-connection (make-spec (postgres-db-name db)
                                         (postgres-locator db))
    (let ((conn (make-instance 'connection :impl postmodern:*database*)))
      (funcall fn conn))))
  
(defmethod mita.db:call-with-tx ((conn connection) fn)
  (postmodern:with-transaction (nil :serializable)
    (funcall fn)))

(defun execute (conn query-string args)
  (let ((conn-impl (connection-impl conn)))
    (cl-postgres:prepare-query
     conn-impl "" query-string)
    (cl-postgres:exec-prepared
     conn-impl "" args #'cl-postgres:list-row-reader)))

;;;

(defun create-admin-database (postgres-dir db-name locator)
  (postmodern:with-connection (make-spec db-name locator)
    (postmodern:execute-file
     (merge-pathnames postgres-dir "./admin-ddl.sql"))))

;;;

(defun create-database (postgres-dir admin-db-name db-name locator)
  (postmodern:with-connection (make-spec admin-db-name locator)
    (postmodern:query (format nil "CREATE DATABASE ~A" db-name)))
  (postmodern:with-connection (make-spec db-name locator)
    (postmodern:execute-file
     (merge-pathnames postgres-dir "./mita-ddl.sql"))))

(defun drop-database (admin-db-name db-name locator)
  (postmodern:with-connection (make-spec admin-db-name locator)
    (postmodern:query (format nil "DROP DATABASE IF EXISTS ~A" db-name))))

;;;

(defmethod mita.db:page-text-update ((conn connection)
                                     (page-id mita.id:id)
                                     (text string))
  (execute conn
   "UPDATE page_text set string = $1 where page_id = $2"
   (list text (mita.id:to-string page-id))))

(defmethod mita.db:album-select-album-ids ((conn connection) offset limit)
  (mapcar (lambda (row)
            (mita.id:parse (car row)))
          (execute conn
           (concatenate 'string
            "SELECT album_id FROM albums"
            " ORDER BY created_on DESC OFFSET $1 LIMIT $2")
           (list offset limit))))

(defmethod mita.db:tag-update ((conn connection)
                               (tag-id mita.id:id)
                               (name string))
  (execute conn
   "UPDATE tags SET name = $1 where tag_id = $2"
   (list name (mita.id:to-string tag-id))))

;;;

(defmethod mita.db.relational:timestamp-to-string ((conn connection)
                                                   (ts local-time:timestamp))
  (local-time:to-rfc3339-timestring ts))

(defmethod mita.db.relational:parse-timestamp ((conn connection) val)
  (local-time:universal-to-timestamp val))

(defmethod mita.db.relational:insert-into ((conn connection)
                                           table-name
                                           column-name-list
                                           values-list)
  (execute conn
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

(defmethod mita.db.relational:delete-from ((conn connection) table-name
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
      (execute conn query-string args))))

(defmethod mita.db.relational:select-from ((conn connection)
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
      (execute conn query-string args))))
