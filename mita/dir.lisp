(defpackage :mita.dir
  (:use :cl)
  (:export :file
           :file-name
           :file-path
           :file-full-path
           :file-dir-p
           :file-dir-list
           :as-file
           :list-dirs))
(in-package :mita.dir)

(defstruct file root full-path dir-p)

(defun as-file (root path)
  (let ((dir-p (and (cl-fad:directory-pathname-p path) t)))
    (make-file :root root
               :full-path path
               :dir-p dir-p)))

(defun subtract-pathname (root path)
  (setq root (namestring root))
  (setq path (namestring path))
  (if (and (< (length root) (length path))
           (string= root path :end2 (length root)))
      (subseq path (length root))
      path))

(defun file-dir-list (file)
  (when (file-dir-p file)
    (let ((child-files
           (mapcar (lambda (p)
                     (as-file (file-root file) p))
                   (cl-fad:list-directory (file-full-path file)))))
      child-files)))

(defun file-name (file)
  (let ((p (cl-fad:pathname-as-file (file-full-path file))))
    (if (file-dir-p file)
        (pathname-name p)
        (format nil "~A.~A"
                (pathname-name p)
                (pathname-type p)))))

(defun file-path (file)
  (subtract-pathname (cl-fad:pathname-as-file
                      (file-root file))
                     (file-full-path file)))


(defun list-dirs (root path)
  (when (cl-fad:directory-pathname-p path)
    (let ((sub-dirs
           (remove-if-not #'cl-fad:directory-pathname-p
                          (cl-fad:list-directory path))))
      (cons (as-file root path)
            (alexandria:mappend (lambda (p)
                                  (list-dirs root p))
                                sub-dirs)))))
