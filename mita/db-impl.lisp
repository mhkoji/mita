(defpackage :mita.db.impl
  (:use :cl)
  (:export :init-db
           :make-connector))
(in-package :mita.db.impl)

(defun reexport (syms)
  (dolist (sym syms)
    (import sym)
    (export (intern (string sym)))))

(progn
  (defun make-connector ()
    (mita.postgres:make-connector
     :user "postgres"
     :host "localhost"
     :port 5432))

  (defun system-relative-pathname (name)
    (asdf:system-relative-pathname (asdf:find-system :mita) name))
  
  (defun init-db (&key (drop-p nil)
                       (connector)
                       (postgres-dir
                        (system-relative-pathname "../postgres/")))
    (mita.postgres:init postgres-dir connector drop-p))

  (reexport '(mita.postgres:with-db
              mita.postgres:with-admin-db)))

#+nil
(progn
  (defun make-connector ()
    (mita.db.file:make-connector :dir "./db-file/"))

  (defun init-db (connector)
    (mita.db.file:init connector))

  (reexport '(mita.db.file:with-db
              mita.db.file:with-admin-db)))
