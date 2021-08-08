(defpackage :mita.db.vendor.mysql
  (:use :cl)
  (:export :make-locator
           :mysql
           :connection
           :create-database
           :drop-database))
(in-package :mita.db.vendor.mysql)

(defstruct locator user host port)

(progn
  (remhash :timestamp cl-mysql:*type-map*)
  (remhash :date      cl-mysql:*type-map*)
  (remhash :time      cl-mysql:*type-map*)
  (remhash :datetime  cl-mysql:*type-map*)
  (remhash :newdate   cl-mysql:*type-map*))

(defclass mysql (mita.db:db)
  ((db-name
    :initarg :db-name
    :reader mysql-db-name)
   (locator
    :initarg :locator
    :reader mysql-locator)))

(defclass connection (mita.db.rdb:connection)
  ((impl
    :initarg :impl
    :reader connection-impl)))

(defmethod mita.db:call-with-connection ((db mysql) fn)
  (let ((locator (mysql-locator db)))
    (mita.util.mysql:call-with-connection
     (lambda (conn-impl)
       (funcall fn (make-instance 'connection :impl conn-impl)))
     (mysql-db-name db)
     (locator-user locator)
     (locator-host locator)
     (locator-port locator))))
  
(defmethod mita.db:call-with-tx ((conn connection) fn)
  (mita.util.mysql:call-with-tx (connection-impl conn) fn))

(defun execute (conn query-string args)
  (mita.util.mysql:execute (connection-impl conn) query-string args))

;;;

(defun to-mysql-timestamp-string (timestamp)
  (local-time:format-timestring nil timestamp
   :format '((:year 4) #\- (:month 2) #\- (:day 2) #\space
             (:hour 2) #\: (:min 2) #\: (:sec 2) #\. (:nsec 9))))

(defun parse-mysql-timestamp-string (string)
  (local-time:parse-timestring string :date-time-separator #\Space))

(defmethod mita.db.vendor.rdb:timestamp-to-string ((conn connection)
                                                   (ts local-time:timestamp))
  (to-mysql-timestamp-string ts))

(defmethod mita.db.vendor.rdb:parse-timestamp ((conn connection)
                                               (s string))
  (parse-mysql-timestamp-string s))

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

(defmethod mita.db.vendor.rdb:delete-from ((conn connection)
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

(defmethod mita.db.vendor.rdb:select-from ((conn connection)
                                           column-names
                                           table-name
                                           &key where
                                                order-by
                                                limit)
  (assert (null limit))
  (destructuring-bind (query-string args)
      (convert-select-query column-names table-name where order-by)
    (let ((plists (execute conn query-string args)))
      (mapcar (lambda (plist)
                (mapcar #'cdr (alexandria:plist-alist plist)))
              plists))))

(defmethod mita.db.rdb:album-select-album-ids ((conn connection) offset limit)
  (mapcar (lambda (plist)
            (let ((row (mapcar #'cdr (alexandria:plist-alist plist))))
              (mita.id:parse (car row))))
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

;;;

(defun create-database (locator db-name ddl-file-path)
  (mita.db:with-connection (conn (make-instance 'mysql
                                  :db-name nil
                                  :locator locator))
    (cl-dbi:do-sql (connection-impl conn)
      (format nil "CREATE DATABASE IF NOT EXISTS ~A" db-name)))
  (mita.db:with-connection (conn (make-instance 'mysql
                                  :db-name db-name
                                  :locator locator))
    (dolist (sql (cl-ppcre:split (format nil "~%~%")
                                 (alexandria:read-file-into-string ddl-file-path)))
      (cl-dbi:do-sql (connection-impl conn) sql))))

(defun drop-database (locator db-name)
  (mita.db:with-connection (conn (make-instance 'mysql
                                  :db-name nil
                                  :locator locator))
    (cl-dbi:do-sql (connection-impl conn)
      (format nil "DROP DATABASE IF EXISTS ~A" db-name))))
