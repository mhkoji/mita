(defpackage :mita.docker.web
  (:use :cl :mita.docker.config)
  (:export :main))
(in-package :mita.docker.web)
(ql:quickload '(:mita-web-backend
                :mita-web-backend-aserve))

(defun clack ()
  (mita.web.clack:start
   :port 5001
   :static-root *static-root*
   :content-base *content-base*
   :thumbnail-base *thumbnail-base*
   :session-store *session-store*
   ;; This server halts by broken pippes errors occurred when there is a number of accesses for images.
   ;; Thus we use aserve instead.
   :serve-image nil
   :db-manager *db-manager*
   :use-thread nil))

(defun aserve ()
  (mita.web.aserve:start
   :port 5003
   :content-base *content-base*
   :thumbnail-base *thumbnail-base*
   :session-store *session-store*
   :db-manager *db-manager*)
  (loop do (sleep 1000)))

(defun init ()
  (mita.auth.admin:init
   *db-manager*
   *content-base*
   *thumbnail-base*))


#+sbcl
(defun main ()
  (let ((cmd (second sb-ext:*posix-argv*)))
    (cond ((string= cmd "clack") (clack))
          ((string= cmd "aserve") (aserve))
          ((string= cmd "init") (init))
          (t (error "no such command: ~A" cmd)))))
