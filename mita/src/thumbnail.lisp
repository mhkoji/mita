(defpackage :mita.thumbnail
  (:use :cl)
  (:export :*convert-path*
           :create))
(in-package :mita.thumbnail)

(defvar *convert-path* "/usr/bin/convert")

(defun resize-image (target source x y)
  (let ((resize (format nil "~Ax~A" x y)))
    ;; call ImageMagic
    #+sbcl
    (sb-ext:run-program *convert-path*
                        (list source "-resize" resize target)
                        :output t :error t)
    #+ccl
    (ccl:run-program "/usr/bin/convert"
                     (list source "-resize" resize target)
                     :output t :error t)))


(defun create (thumbnail-path source-path)
  (setq source-path (namestring source-path))
  (setq thumbnail-path (namestring thumbnail-path))
  (unless (cl-fad:file-exists-p thumbnail-path)
    (resize-image thumbnail-path source-path 300 300))
  thumbnail-path)
