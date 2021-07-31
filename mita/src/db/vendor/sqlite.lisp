(defpackage :mita.db.vendor.sqlite
  (:use :cl)
  (:export :make-locator
           :sqlite
           :connection
           :create-database
           :drop-database))
(in-package :mita.db.vendor.sqlite)

(defstruct locator path)

(defclass sqlite (mita.db:db)
  ((locator
    :initarg :locator
    :reader sqlite-locator)))

(defclass connection (mita.db.rdb:connection)
  ((impl
    :initarg :impl
    :reader connection-impl)))

(defmethod mita.db:call-with-connection ((db sqlite) fn)
  (sqlite:with-open-database (impl (locator-path (sqlite-locator db)))
    (funcall fn (make-instance 'connection :impl impl))))

(defmethod mita.db:call-with-tx ((conn connection) fn)
  (sqlite:with-transaction (connection-impl conn)
    (funcall fn)))

(defun execute (conn query-string args)
  (apply #'sqlite:execute-to-list (connection-impl conn) query-string args))

;;;

(defun to-sqlite-timestamp-string (timestamp)
  (local-time:format-timestring nil timestamp
   :format '((:year 4) #\- (:month 2) #\- (:day 2) #\space
             (:hour 2) #\: (:min 2) #\: (:sec 2) #\. (:nsec 9))))

(defun parse-sqlite-timestamp-string (string)
  (local-time:parse-timestring string :date-time-separator #\Space))

(defmethod mita.db.rdb.common:timestamp-to-string ((conn connection)
                                                   (ts local-time:timestamp))
  (to-sqlite-timestamp-string ts))

(defmethod mita.db.rdb.common:parse-timestamp ((conn connection)
                                               (s string))
  (parse-sqlite-timestamp-string s))

(defmethod mita.db.rdb.common:insert-into ((conn connection)
                                           table-name
                                           column-name-list
                                           values-list)
  (execute conn
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
                   ((:and :or)
                    (let ((op (car clause)))
                      (destructuring-bind (left right) (cdr clause)
                        (rec left
                         (lambda (l-cond l-acc-values)
                           (rec right
                            (lambda (r-cond r-acc-values)
                              (funcall k
                               (format nil "~A ~A ~A" l-cond op r-cond)
                               (append l-acc-values r-acc-values)))))))))
                   ((:fn)
                    (let ((name (second clause))
                          (arg (third clause)))
                      (rec arg
                        (lambda (cond acc-values)
                          (funcall k
                            (format nil "~A(~A)" name cond)
                            acc-values)))))
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

(defmethod mita.db.rdb.common:delete-from ((conn connection)
                                           table-name
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

(defun convert-select-query (column-names table-name where order-by)
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
      (list query-string args))))

(defmethod mita.db.rdb.common:select-from ((conn connection)
                                           column-names
                                           table-name
                                           &key where
                                                order-by)
  (destructuring-bind (query-string args)
      (convert-select-query column-names table-name where order-by)
    (execute conn query-string args)))

;;;

(defun create-database (sqlite-dir locator)
  (mita.db:with-connection (conn (make-instance 'sqlite :locator locator))
    (dolist (sql (cl-ppcre:split
                  #+windows
                  (coerce '(#\Return #\Newline #\Return #\Newline) 'string)
                  #+linux
                  (format nil "~%~%")
                  (alexandria:read-file-into-string
                   (merge-pathnames sqlite-dir "./mita-ddl.sql"))))
      (sqlite:execute-non-query (connection-impl conn) sql))))

(defun drop-database (locator)
  (uiop:delete-file-if-exists (locator-path locator)))

;;;

(defmethod mita.db.rdb:album-select-album-ids ((conn connection) offset limit)
  (mapcar (lambda (row)
            (mita.id:parse (car row)))
          (execute conn
           (concatenate 'string
            "SELECT album_id FROM albums"
            " ORDER BY created_on DESC LIMIT ?, ?")
           (list offset limit))))

(defmethod mita.db.rdb:tag-update ((conn connection)
                                   (tag-id mita.id:id)
                                   (name string))
  (execute conn
   "UPDATE tags SET name = ? where tag_id = ?"
   (list name (mita.id:to-string tag-id))))
