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

(defmethod mita.db.vendor.rdb:timestamp-to-string ((conn connection)
                                                   (ts local-time:timestamp))
  (to-sqlite-timestamp-string ts))

(defmethod mita.db.vendor.rdb:parse-timestamp ((conn connection)
                                               (s string))
  (parse-sqlite-timestamp-string s))

(defmethod mita.db.vendor.rdb:insert-into ((conn connection)
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

(defun get-place-holder (value-count)
  (format nil "(~{~A~^,~})" (make-list
                             value-count
                             :initial-element "?")))

(defun parse-where-condition (clause)
  (mita.util.rdb:parse-where-condition clause
   :get-place-holder-fn #'get-place-holder))

(defun parse-limit (offset count)
  (destructuring-bind (ofs ofs-value)
      (mita.util.rdb:parse-expr offset
       :get-place-holder-fn #'get-place-holder)
    (destructuring-bind (cnt cnt-value)
        (mita.util.rdb:parse-expr count
         :get-place-holder-fn #'get-place-holder)
      (list (format nil "~A,~A" ofs cnt) (append ofs-value cnt-value)))))
  
(defmethod mita.db.vendor.rdb:delete-from ((conn connection)
                                           table-name
                                           &key where)
  (let ((args nil))
    (let ((query-string
           (with-output-to-string (s)
             (format s "DELETE FROM ~A" table-name)
             (when where
               (destructuring-bind (cond-string values)
                   (parse-where-condition where)
                 (format s " WHERE ~A" cond-string)
                 (alexandria:appendf args values))))))
      (execute conn query-string args))))

(defun convert-select-query (column-names table-name
                             where order-by limit)
  (let ((args nil))
    (let ((query-string
           (with-output-to-string (s)
             (format s "SELECT ~A FROM ~A" column-names table-name)
             (when where
               (destructuring-bind (cond-string vals)
                   (parse-where-condition where)
                 (format s " WHERE ~A" cond-string)
                 (alexandria:appendf args vals)))
             (when order-by
               (format s " ORDER BY ~A" order-by))
             (when limit
               (destructuring-bind (offset count) limit
                 (destructuring-bind (limit-string vals)
                     (parse-limit offset count)
                   (format s " LIMIT ~A" limit-string)
                   (alexandria:appendf args vals))))
             )))
      (list query-string args))))

(defmethod mita.db.vendor.rdb:select-from ((conn connection)
                                           column-names
                                           table-name
                                           &key where
                                                order-by
                                                limit)
  (destructuring-bind (query-string args)
      (convert-select-query column-names table-name where order-by limit)
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

(defmethod mita.db.rdb:tag-update ((conn connection)
                                   (tag-id mita.id:id)
                                   (name string))
  (execute conn
   "UPDATE tags SET name = ? where tag_id = ?"
   (list name (mita.id:to-string tag-id))))
