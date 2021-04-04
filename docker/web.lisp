(defpackage :mita.docker.web
  (:use :cl :mita.docker.config)
  (:export :main))
(in-package :mita.docker.web)
(ql:quickload '(:mita-server
                :mita-server-aserve))

(defun clack ()
  (mita.server.clack:start
   :port 5001
   :static-root *static-root*
   :content-base *content-base*
   :thumbnail-base *thumbnail-base*
   :session-store *session-store*
   ;; This server halts by broken pippes errors occurred when there is a number of accesses for images.
   ;; Thus we use aserve instead.
   :serve-image nil
   :locator *locator*
   :use-thread nil))

(defun aserve ()
  (mita.server.aserve:start
   :port 5003
   :content-base *content-base*
   :thumbnail-base *thumbnail-base*
   :session-store *session-store*
   :locator *locator*)
  (loop do (sleep 1000)))

(defun init ()
  (mita.admin:init
   *locator*
   "/root/quicklisp/local-projects/mita/mysql/"
   *content-base*
   *thumbnail-base*))


#+sbcl
(defun main ()
  (let ((cmd (second sb-ext:*posix-argv*)))
    (cond ((string= cmd "clack") (clack))
          ((string= cmd "aserve") (aserve))
          ((string= cmd "init") (init))
          (t (error "no such command: ~A" cmd)))))
