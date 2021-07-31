(defpackage :mita.docker.web
  (:use :cl :mita.docker.config)
  (:export :main))
(in-package :mita.docker.web)
(ql:quickload '(:mita-delivery-web-full-mita
                :mita-delivery-web-full-mita-image))

(defun clack ()
  (mita.delivery.web.full.server:start
   :port 5001
   :session-store *session-store*
   :db-manager *db-manager*
   :static-root *static-root*
   :content-base *content-base*
   :thumbnail-base *thumbnail-base*
   ;; This server halts by broken pippes errors occurred when there is a number of accesses for images.
   ;; Thus we use aserve instead.
   :serve-image nil
   :use-thread nil))

(defun aserve ()
  (mita.delivery.web.full.server-image:start
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
