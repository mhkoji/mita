(defpackage :mita.util.threading
  (:use :cl)
  (:export :->
           :->>))
(in-package :mita.util.threading)

(defun simple-inserter (insert-fn)
  (lambda (acc next)
    (if (listp next)
        (funcall insert-fn acc next)
        (list next acc))))

(defun insert-first (arg surround)
  (list* (car surround) arg (cdr surround)))

(defun insert-last (arg surround)
  (append surround (list arg)))

(defmacro -> (initial-form &rest forms)
  (reduce (simple-inserter #'insert-first) forms
          :initial-value initial-form))

(defmacro ->> (initial-form &rest forms)
  (reduce (simple-inserter #'insert-last) forms
          :initial-value initial-form))
