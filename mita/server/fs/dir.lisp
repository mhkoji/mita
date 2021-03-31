(defpackage :mita.fs.dir
  (:use :cl :mita.fs)
  (:shadow :file)
  (:export :file-full-path
           :as-file
           :list-dirs))
(in-package :mita.fs.dir)

(defstruct file-impl root full-path dir-p)

;; /home/xxx/
;;          + a/
;;             + b/
;;
;; (file-impl "/home/xxx/" "/home/xxx/a/b/")
;; -> full-path:  /home/xxx/a/b/
;; -> path:                /a/b/

(defun mk-file-impl (root path)
  (let ((dir-p (and (cl-fad:directory-pathname-p path) t)))
    (make-file-impl :root (namestring root)
                    :full-path (namestring path)
                    :dir-p dir-p)))

(defun subtract-pathname (root path)
  (if (and (< (length root) (length path))
           (string= root path :end2 (length root)))
      (subseq path (length root))
      path))

(defclass file (mita.fs:file)
  ((impl
    :initarg :impl
    :reader impl)))

(defun as-file (root path)
  (make-instance 'file :impl (mk-file-impl root path)))
    
(defmethod file-name ((file file))
  (let ((impl (impl file)))
    (let ((p (cl-fad:pathname-as-file (file-impl-full-path impl))))
      (if (file-impl-dir-p impl)
          (pathname-name p)
          (format nil "~A.~A"
                  (pathname-name p)
                  (pathname-type p))))))

(defmethod file-path ((file file))
  (let ((impl (impl file)))
    (subtract-pathname
     (namestring (cl-fad:pathname-as-file (file-impl-root impl)))
     (file-impl-full-path impl))))

(defmethod dir-p ((file file))
  (file-impl-dir-p (impl file)))

(defmethod dir-list-children ((file file))
  (let ((impl (impl file)))
    (when (file-impl-dir-p impl)
      (let ((child-files
             (mapcar (lambda (p)
                       (as-file (file-impl-root impl) p))
                     (cl-fad:list-directory (file-impl-full-path impl)))))
        child-files))))

(defun list-dirs (root path)
  (when (cl-fad:directory-pathname-p path)
    (let ((sub-dirs (remove-if-not #'cl-fad:directory-pathname-p
                                   (cl-fad:list-directory path))))
      (cons (as-file root path)
            (alexandria:mappend (lambda (p) (list-dirs root p))
                                sub-dirs)))))

(defmethod file-created-on ((file file))
  (local-time:universal-to-timestamp
   (file-write-date (file-impl-full-path (impl file)))))

(defmethod file-size ((file file))
  (with-open-file (in (file-impl-full-path (impl file))
                      :element-type '(unsigned-byte 8))
    (file-length in)))

(defun file-full-path (file)
  (file-impl-full-path (impl file)))

(defmethod mita.fs:make-thumbnail ((dir file) (source-image-file file))
  (assert (dir-p dir))
  (let ((thumbnail-full-path
         (concatenate 'string
          (file-impl-full-path (impl dir))
          "/"
          (cl-ppcre:regex-replace-all
           "/"
           (concatenate 'string "thumbnail" (file-path source-image-file))
           "$")))
        (source-image-full-path
         (file-impl-full-path (impl source-image-file))))
    (mita.thumbnail:create thumbnail-full-path source-image-full-path)
    (as-file (file-impl-full-path (impl dir)) thumbnail-full-path)))
