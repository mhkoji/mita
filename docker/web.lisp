(defpackage :mita.docker.web
  (:use :cl)
  (:export :main))
(in-package :mita.docker.web)
(ql:quickload '(:mita-server
                :mita-server-aserve))

(defvar *connector*
  (mita.postgres:make-connector
   :user "postgres"
   :host "postgres"
   :port 5432))

(defvar *sesson-store*
  (mita.auth.session:make-redis-store
   :host "redis"))


(defun clack ()
  (mita.server.clack:start
   :port 5001
   :static-root "/app/static/"
   :content-root "/data/albums/"
   :thumbnail-root "/data/thumbnails/"
   :session-store *sesson-store*
   ;; This server halts by broken pippes errors occurred when there is a number of accesses for images.
   ;; Thus we use aserve instead.
   :serve-image nil
   :connector *connector*
   :use-thread nil))

(defun aserve ()
  (mita.server.aserve:start
   :port 5003
   :content-root "/data/albums/"
   :thumbnail-root "/data/thumbnails/"
   :session-store *sesson-store*
   :connector *connector*)
  (loop do (sleep 1000)))

(defun init ()
  (mita.admin:init
   "/root/quicklisp/local-projects/mita/postgres/"
   *connector*
   nil))

#+sbcl
(defun main ()
  (let ((cmd (second sb-ext:*posix-argv*)))
    (cond ((string= cmd "clack") (clack))
          ((string= cmd "aserve") (aserve))
          ((string= cmd "init") (init))
          (t (error "no such command: ~A" cmd)))))
