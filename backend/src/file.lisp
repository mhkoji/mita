(defpackage :mita.file
  (:use :cl)
  (:export :file
           :file-path
           :file-size
           :file-full-path
           :folder
           :folder-p
           :folder-list-files
           :os-list-files
           :os-make-file
           :make-loader))
(in-package :mita.file)

(defclass file ()
  ((path
    :initarg :path
    :reader file-path)
   (full-path
    :initarg :full-path
    :reader file-full-path)))

(defun file-size (file)
  (with-open-file (in (file-full-path file)
                      :element-type '(unsigned-byte 8))
    (file-length in)))

(defclass folder (file) ())

(defun folder-p (file)
  (typep file 'folder))

;; /home/xxx/
;;          + a/
;;             + b/
;;
;; root:  /home/xxx/
;; full:  /home/xxx/a/b/
;; path:           /a/b/
(defun namestring-subtract (root-namestring full-namestring)
  (if (and (< (length root-namestring)
              (length full-namestring))
           (string= root-namestring
                    full-namestring
                    :end2 (length root-namestring)))
      (subseq full-namestring (length root-namestring))
      full-namestring))

(defun make-file (path full-path)
  (make-instance (if (uiop/pathname:directory-pathname-p full-path)
                     'folder 'file)
                 :path path
                 :full-path full-path))

(defun os-list-files (root-path base-path)
  (let ((root-namestring (namestring root-path)))
    (loop for full-path in (uiop/filesystem:with-current-directory
                               (base-path)
                             (uiop/filesystem:directory*
                              uiop/pathname:*wild-file-for-directory*))
          for path = (uiop/pathname:relativize-pathname-directory
                      (namestring-subtract root-namestring
                                           (namestring full-path)))
          collect (make-file path full-path))))

(defun os-make-file (root-path namestring)
  (let ((path (uiop/pathname:relativize-pathname-directory namestring)))
    (let ((full-path (uiop/pathname:ensure-absolute-pathname
                      path (merge-pathnames root-path))))
      (make-file path full-path))))

;;;

(defun folder-list-files (folder root-path)
  (os-list-files root-path (file-full-path folder)))
