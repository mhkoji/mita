(defpackage :mita.web.file
  (:use :cl)
  (:export :get-file
           :add-files
           :delete-file
           :add-albums)
  (:shadow :delete-file))
(in-package :mita.web.file)

(defun content-repos (dep req)
  (make-instance 'mita.file.fs:repository
                 :root (mita.web.dep:get-content-root dep req)))

(defun thumbnail-repos (dep req)
  (make-instance 'mita.file.fs:repository
                 :root (mita.web.dep:get-thumbnail-root dep req)))


(defun get-file (dep req path &key on-folder on-file on-not-found)
  (if (not path)
      (funcall on-not-found)
      (let ((content-repos (content-repos dep req)))
        (let ((file (mita.file.fs:as-file content-repos path)))
          (cond ((not file)
                 (funcall on-not-found))
                ((mita.file:folder-p file)
                 (let ((files (mita.file:folder-list-children content-repos file)))
                   (funcall on-folder file files)))
                (t
                 (funcall on-file (mita.file.fs:file-full-path file))))))))

;; todo: directory traversal
(defun add-files (dep req parent-dir files)
  (let ((dir-full-path (merge-pathnames
                        parent-dir
                        (mita.web.dep:get-content-root dep req))))
    (loop for (path stream) in files do
      (let ((file-full-path (merge-pathnames path dir-full-path)))
        (ensure-directories-exist file-full-path)
        (with-open-file (out file-full-path
                             :direction :output
                             :element-type '(unsigned-byte 8)
                             :if-does-not-exist :create
                             :if-exists :overwrite)
          (alexandria:copy-stream stream out))))))

;; todo: directory traversal
(defun delete-file (dep req dir-path)
  (let ((dir-full-path (merge-pathnames
                        dir-path
                        (mita.web.dep:get-content-root dep req))))
    (uiop:delete-directory-tree dir-full-path
                                :validate t ;; todo
                                :if-does-not-exist :ignore)))


(defun add-albums (dep req path)
  (let ((content-repos (content-repos dep req))
        (thumbnail-repos (thumbnail-repos dep req)))
    (let ((folders (mita.file.fs:list-folders content-repos path)))
      (when folders
        (mita.db:with-connection (conn (mita.web.dep:get-db dep req))
          (mita.db:with-tx (conn)
            (mita.add-albums:run conn thumbnail-repos content-repos folders)))))))
