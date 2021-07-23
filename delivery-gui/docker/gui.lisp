(defpackage :mita.docker.gui
  (:use :cl :mita.docker.config)
  (:export :main))
(in-package :mita.docker.gui)
(ql:quickload '(:mita-gui-backend))

(defun clack ()
  (mita.gui.clack:start
   :use-thread nil
   :static-root *static-root*
   :content-base *content-base*
   :thumbnail-base *thumbnail-base*
   :locator *locator*))

#+sbcl
(defun main ()
  (let ((cmd (second sb-ext:*posix-argv*)))
    (cond ((string= cmd "clack") (clack))
          (t (error "no such command: ~A" cmd)))))
