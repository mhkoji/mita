(defpackage :mita.file.fs
  (:use :cl :mita.file)
  (:shadow :file :repository)
  (:export :file
           :file-full-path
           :repository
           :as-file
           :list-folders))
(in-package :mita.file.fs)

(defclass repository (mita.file:repository)
  ((root :initarg :root
         :reader repository-root)))

(defclass file (mita.file:file)
  ((root
    :initarg :root
    :reader file-root)
   (full-path
    :initarg :full-path
    :reader file-full-path)))

(defun full-path (repos path)
  (concatenate 'string (repository-root repos) (namestring path)))

(defun as-file (repos path)
  (let ((full-path (full-path repos path)))
    (when (cl-fad:file-exists-p full-path)
      (make-instance 'file :root (repository-root repos) :full-path full-path))))

;; /home/xxx/
;;          + a/
;;             + b/
;;
;; root:       /home/xxx/
;; path:                /a/b/
;; full-path:  /home/xxx/a/b/

(defun subtract-pathname (root full-path)
  (if (and (< (length root) (length full-path))
           (string= root full-path :end2 (length root)))
      (subseq full-path (length root))
      full-path))

(defmethod file-path ((file file))
  (subtract-pathname (namestring (cl-fad:pathname-as-file (file-root file)))
                     (namestring (file-full-path file))))

(defmethod folder-p ((file file))
  (when (cl-fad:directory-pathname-p (file-full-path file))
    t))

(defmethod file-name ((file file))
  (let ((p (cl-fad:pathname-as-file (file-full-path file))))
    (if (folder-p file)
        (pathname-name p)
        (format nil "~A.~A" (pathname-name p) (pathname-type p)))))

(defmethod folder-list-children ((repos repository) (file file))
  (when (folder-p file)
    (let ((root (repository-root repos)))
      (mapcar (lambda (p)
                (make-instance 'file :root root :full-path p))
              (cl-fad:list-directory (file-full-path file))))))

(defun list-folders (repos path)
  (labels ((rec (full-path)
             (when (and (cl-fad:file-exists-p full-path)
                        (cl-fad:directory-pathname-p full-path))
               (cons (make-instance 'file
                                    :root (repository-root repos)
                                    :full-path full-path)
                     (let ((sub-folders
                            (remove-if-not #'cl-fad:directory-pathname-p
                                           (cl-fad:list-directory full-path))))
                       (alexandria:mappend #'rec sub-folders))))))
    (rec (full-path repos path))))

(defmethod file-created-on ((file file))
  (local-time:universal-to-timestamp (file-write-date (file-full-path file))))

(defmethod file-size ((file file))
  (with-open-file (in (file-full-path file)
                      :element-type '(unsigned-byte 8))
    (file-length in)))

(defmethod mita.file:make-thumbnail ((repos repository)
                                     (source-image-file file))
  (let ((thumbnail-path
         (cl-ppcre:regex-replace-all
          "/"
          (concatenate 'string "thumbnail" (file-path source-image-file))
          "$")))
    (mita.thumbnail:create (full-path repos thumbnail-path)
                           (file-full-path source-image-file))
    (as-file repos thumbnail-path)))
