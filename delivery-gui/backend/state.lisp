;; Commonly used states
(defpackage :mita.gui.state
  (:use :cl)
  (:export :loading
           :make-loading
           :viewing
           :viewing-current-image
           :viewing-images
           :viewing-index
           :viewing-thumbnails
           :make-viewing))
(in-package :mita.gui.state)

(defstruct loading)

(defstruct viewing index)

(defgeneric viewing-images (viewing))

(defun viewing-current-image (viewing)
  (nth (viewing-index viewing) (viewing-images viewing)))

(defun viewing-thumbnails (viewing width)
  (let ((images (viewing-images viewing))
        (len (length (viewing-images viewing))))
    (if (<= len width)
        images
        (let* ((half-width (floor width 2))
               (index (viewing-index viewing))
               (begin (- index half-width)))
          (cond ((< begin 0)
                 (subseq images 0 width))
                ((<= (+ begin width) len)
                 (subseq images begin (+ begin width)))
                (t
                 (subseq images (- len width) len)))))))
