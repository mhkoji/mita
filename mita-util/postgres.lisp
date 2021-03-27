(defpackage :mita.util.postgres
  (:use :cl)
  (:export :make-connector
           :connector
           :postgres
           :with-db
           :execute
           :insert-into
           :select-from
           :delete-from
           :single))
(in-package :mita.util.postgres)

(defstruct connector user host port)

(defun connector->spec (database connector)
  (list database
        (connector-user connector)
        "" ;; password
        (connector-host connector)
        :port (connector-port connector)
        :pooled-p t))

(defclass postgres ()
  ((conn
    :initarg :conn
    :reader postgres-conn)))

(defmacro with-db ((db db-name connector) &body body)
  `(postmodern:with-connection (connector->spec ,db-name ,connector)
     (postmodern:with-transaction (nil :serializable)
       (let ((,db (make-instance 'postgres :conn postmodern:*database*)))
         ,@body))))

(defun execute (db query-string args)
  (let ((conn (postgres-conn db)))
    (cl-postgres:prepare-query conn "" query-string)
    (cl-postgres:exec-prepared conn "" args #'cl-postgres:list-row-reader)))

(defun insert-into (db table-name column-name-list values-list)
  (execute db
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

(defun delete-from (db table-name cond)
  (destructuring-bind (cond-string values) (parse-clause cond)
    (execute db
             (format nil "DELETE FROM ~A ~A"
                     table-name
                     cond-string)
             values)))

(defun select-from (db column-names table-name cond &key order-by)
  (destructuring-bind (cond-string values) (parse-clause cond)
    (execute db
             (with-output-to-string (s)
               (format s "SELECT ~A FROM ~A ~A"
                       column-names
                       table-name
                       cond-string)
               (when order-by
                 (format s "ORDER BY ~A" order-by)))
             values)))

(defun single (row-parser select-result)
  (car (mapcar row-parser select-result)))
