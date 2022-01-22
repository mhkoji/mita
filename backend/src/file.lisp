(defpackage :mita.file
  (:use :cl)
  (:export :file
           :file-path
           :file-size
           :file-full-path
           :file-created-at
           :folder
           :folder-p
           :folder-list-files
           :store-list-files
           :store-make-file
           :store-prepare-cache
           :make-store))
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

(defun file-created-at (file)
  (file-write-date (file-full-path file)))

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

(defstruct store
  root-path
  (sort-file-fn #'identity)
  (list-files-cache (make-hash-table :test #'equal))
  (list-files-cache-mutex (bt:make-lock "list-files-cache-mutex")))

(defun make-file (path full-path)
  (make-instance (if (uiop/pathname:directory-pathname-p full-path)
                     'folder 'file)
                 :path path
                 :full-path full-path))

(defun store-list-files-from-cache-or-update (store key list-fn)
  (bt:with-lock-held ((store-list-files-cache-mutex store))
    (let ((cache (store-list-files-cache store)))
      (copy-list (or (gethash key cache)
                     (setf (gethash key cache)
                           (funcall list-fn)))))))

(defun store-list-files (store path)
  (store-list-files-from-cache-or-update store (namestring path)
   (lambda ()
     (let ((root-namestring
            (namestring (store-root-path store)))
           (full-path-list
            (funcall (store-sort-file-fn store)
                     (uiop/filesystem:with-current-directory (path)
                       (uiop/filesystem:directory*
                        uiop/pathname:*wild-file-for-directory*)))))
       (loop for full-path in full-path-list
             for path = (uiop/pathname:relativize-pathname-directory
                         (namestring-subtract root-namestring
                                              (namestring full-path)))
             collect (make-file path full-path))))))

(defun store-make-file (store namestring)
  (let ((path (uiop/pathname:relativize-pathname-directory namestring)))
    (let ((full-path (uiop/pathname:ensure-absolute-pathname
                      path (parse-namestring (store-root-path store)))))
      (make-file path full-path))))

(defun store-prepare-cache (store)
  (let ((path-list (list (store-root-path store))))
    (loop while path-list do
      (let ((path (pop path-list)))
        (store-list-files store path)
        (let ((next-path-list
               (remove-if-not
                #'uiop/pathname:directory-pathname-p
                (uiop/filesystem:with-current-directory (path)
                  (uiop/filesystem:directory*
                   uiop/pathname:*wild-file-for-directory*)))))
          (alexandria:appendf path-list next-path-list))))))

;;;

(defun folder-list-files (folder store)
  (store-list-files store (file-full-path folder)))
