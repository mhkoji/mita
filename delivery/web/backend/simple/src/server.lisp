(defpackage :mita.delivery.web.simple
  (:use :cl)
  (:export :start
           :init))
(in-package :mita.delivery.web.simple)

(defun system-relative-pathname (name)
  (asdf:system-relative-pathname (asdf:find-system :mita) name))

(defvar *handler* nil)

(defvar *db-path*
  (namestring
   (system-relative-pathname "../db.sqlite")))

(defvar *static-root*
  (system-relative-pathname "../delivery/web/backend/mita/static/"))

(defvar *content-root*
  (namestring
   (system-relative-pathname
    "../data/content/account_7128da4e_2b13_45cc_a0bf_78aec1668e2c/")))

(defvar *thumbnail-root*
  (namestring
   (system-relative-pathname
    "../data/thumbnail/account_7128da4e_2b13_45cc_a0bf_78aec1668e2c/")))

(defclass spec (mita.web.app:spec) ())

(defmethod mita.web.app:get-db ((spec spec) req)
  (let ((locator (mita.db.vendor.sqlite:make-locator :path *db-path*)))
    (make-instance 'mita.db.vendor.sqlite:sqlite :locator locator)))

(defmethod mita.web.app:get-content-root ((spec spec) req)
  *content-root*)

(defmethod mita.web.app:get-thumbnail-root ((spec spec) req)
  *thumbnail-root*)

(defun start (&key (port 5000)
                   (use-thread t))
  (when *handler*
    (clack:stop *handler*))
  (setq *handler*
        (clack:clackup
         (lack:builder
          (:static :path "/static/" :root *static-root*)

          (mita.util.clack:middleware-log)

          (mita.web.clack:make-middleware (make-instance 'spec) :serve-image-p t)

          (lambda (env)
            (declare (ignore env))
            '(302 (:location "/") nil)))
         :address "0.0.0.0"
	 ;; Don't have to invoke a debugger. No one can take care of it.
	 ;; setq after clackup because clackup set the var to T.
	 :debug nil
         :use-thread use-thread
         :port port)))

(defun init ()
  (mita.db.vendor.sqlite:create-database
   (system-relative-pathname "../sqlite/")
   (mita.db.vendor.sqlite:make-locator :path *db-path*)))
