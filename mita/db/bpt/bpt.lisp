;; b plus tree
(defpackage :mita.db.bpt
  (:use :cl)
  (:export :connection
           :db
           :init-db))
(in-package :mita.db.bpt)

(defclass connection (mita.db:connection)
  ((tree-hash
    :initarg :tree-hash
    :reader connection-tree-hash)))

(defmacro album-tree (conn)
  `(gethash :album (connection-tree-hash ,conn)))

(defun init-db (conn)
  (setf (album-tree conn)
        (mita.util.b+tree:make-tree
         :key< #'string<
         :key= #'string=)))

(defclass p-plus-tree-db (mita.db:db) ())
