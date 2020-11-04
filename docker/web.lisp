(defpackage :mita.docker.web
  (:use :cl)
  (:export :main))
(in-package :mita.docker.web)
(ql:quickload :mita-web :silent t)

(defun main (&rest argv)
  (declare (ignore argv))
  (mita.web.server:start
   :port 5001
   :static-root "/app-output/static/"
   :session-store (lack.session.store.redis:make-redis-store)
   :connector (mita.postgres:make-connector
               :user "postgres"
               :host "localhost"
               :port 5432)
   :use-thread nil))

#+sbcl
(progn
  (export 'sbcl)
  (defun sbcl ()
    (main (cdr sb-ext:*posix-argv*))))
