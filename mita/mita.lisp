(defpackage :mita
  (:use :cl)
  (:export :gateway
           :gateway-db
           :with-gateway
           :create-thumbnail))
(in-package :mita)

(defgeneric gateway-db (gw))

(defclass gateway ()
  ((db
    :initarg :db
    :reader gateway-db)))


;; thumbnail
(defun resize-image (target source x y)
  (let ((resize (format nil "~Ax~A" x y)))
    ;; call ImageMagic
    #+sbcl
    (sb-ext:run-program "/usr/bin/convert"
                        (list source "-resize" resize target)
                        :output t :error t)
    #+ccl
    (ccl:run-program "/usr/bin/convert"
                     (list source "-resize" resize target)
                     :output t :error t)))


(defun create-thumbnail (thumbnail-path source-path)
  (setq source-path (namestring source-path))
  (setq thumbnail-path (namestring thumbnail-path))
  (unless (cl-fad:file-exists-p thumbnail-path)
    (resize-image thumbnail-path source-path 300 300))
  thumbnail-path)
