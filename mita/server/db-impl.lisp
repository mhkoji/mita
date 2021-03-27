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
    (mita.util.postgres:make-connector
     :user "postgres"
     :host "localhost"
     :port 5432))

  (defun system-relative-pathname (name)
    (asdf:system-relative-pathname (asdf:find-system :mita) name))
  
  (reexport '(mita.db.postgres:with-db)))

#+nil
(progn
  (defun make-connector ()
    (mita.db.file:make-connector :dir "./db-file/"))

  (reexport '(mita.db.file:with-db)))

