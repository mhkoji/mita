(defpackage :mita.util.rdb
  (:use :cl)
  (:export :parse-expr
           :parse-where-condition))
(in-package :mita.util.rdb)

(defun parse-expr (clause &key get-place-holder-fn)
  (labels ((rec (clause k)
             (cond ((not (consp clause))
                    (funcall k clause nil))
                   ((not (keywordp (car clause)))
                    (funcall k (format nil "~A" clause) nil))
                   (t
                    (ecase (car clause)
                      (:p
                       (let ((values (alexandria:ensure-list
                                      (second clause))))
                         (let ((place-holder (funcall
                                              get-place-holder-fn
                                              (length values))))
                           (funcall k place-holder values)))))))))
    (rec clause #'list)))


(defun parse-where-condition (clause &key get-place-holder-fn)
  (labels ((rec (clause k)
             (cond ((not (consp clause))
                    (funcall k clause nil))
                   ((not (keywordp (car clause)))
                    (funcall k (format nil "~A" clause) nil))
                   (t
                    (ecase (car clause)
                      ((:and :or)
                       (destructuring-bind (op left right) clause
                         (rec left
                           (lambda (l-cond l-acc-values)
                             (rec right
                               (lambda (r-cond r-acc-values)
                                 (funcall k
                                          (format nil "~A ~A ~A"
                                                  l-cond op r-cond)
                                          (append l-acc-values
                                                  r-acc-values))))))))
                      ((:fn)
                       (let ((name (second clause))
                             (arg (third clause)))
                         (rec arg
                           (lambda (cond acc-values)
                             (funcall k
                                      (format nil "~A(~A)" name cond)
                                      acc-values)))))
                      ((:in := :like)
                       (let ((op (car clause))
                             (column-name (second clause)))
                         (rec (third clause)
                           (lambda (cond acc-values)
                             (funcall k
                                      (format nil "(~A ~A ~A)"
                                              column-name op cond)
                                      acc-values)))))
                      (:p
                       (let ((values (alexandria:ensure-list
                                      (second clause))))
                         (let ((place-holder (funcall
                                              get-place-holder-fn
                                              (length values))))
                           (funcall k place-holder values)))))))))
    (rec clause #'list)))
