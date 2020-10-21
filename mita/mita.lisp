(defpackage :mita
  (:use :cl)
  (:export :gateway
           :gateway-db
           :with-gateway
           :create-thumbnail))
(in-package :mita)

(defgeneric gateway-db (gw))

(defgeneric gateway-account (gw))

(defclass gateway ()
  ((db
    :initarg :db
    :reader gateway-db)
   (configure
    :initarg :configure
    :reader gateway-configure)))


(defun make-thumbnail-path (thumbnail-dir source-path)
  (format nil "~Athumbnail$~A"
          thumbnail-dir
          (cl-ppcre:regex-replace-all "/" source-path "$")))

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


(defun ensure-thumbnail-exists (thumbnail-path source-path)
  (setq source-path (namestring source-path))
  (setq thumbnail-path (namestring thumbnail-path))
  (unless (cl-fad:file-exists-p thumbnail-path)
    (resize-image thumbnail-path source-path 300 300))
  thumbnail-path)

(defun create-thumbnail (thumbnail-dir source-path)
  (ensure-thumbnail-exists (make-thumbnail-path thumbnail-dir source-path)
                           source-path))
