(defpackage :mita.dir
  (:use :cl)
  (:export :dir-path
           :dir-file-paths
           :dir-write-date
           :retrieve))
(in-package :mita.dir)

(defstruct dir path file-paths write-date)

(defun files-and-subdirectories (dir)
  (let ((files nil)
        (subdirectories nil))
    (dolist (child-path (cl-fad:list-directory dir))
      (if (cl-fad:directory-pathname-p child-path)
          (push child-path subdirectories)
          (push (namestring child-path) files)))
    (list (nreverse files) subdirectories)))

(defun retrieve (root sort-paths-fn)
  (destructuring-bind (files subdirs) (files-and-subdirectories root)
    (let ((rest (alexandria:mappend (lambda (p)
                                      (retrieve p sort-paths-fn))
                                    subdirs)))
      (if files
          (cons (make-dir :path (namestring root)
                          :file-paths (funcall sort-paths-fn
                                               (mapcar #'namestring files))
                          :write-date (file-write-date root))
                rest)
          rest))))
