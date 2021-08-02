(defpackage :mita.web.app
  (:use :cl)
  (:export :spec
           :get-db
           :get-content-root
           :get-thumbnail-root
           :image-serve
           :dir-add
           :dir-delete
           :dir-serve
           :dir-add-albums)
  (:import-from :alexandria
                :when-let*))
(in-package :mita.web.app)

(defclass spec () ())

(defgeneric get-db (spec req))

(defgeneric get-content-root (spec req))

(defgeneric get-thumbnail-root (spec req))

;;;;;

(defun content-repos (spec req)
  (make-instance 'mita.file.fs:repository :root (get-content-root spec req)))

(defun thumbnail-repos (spec req)
  (make-instance 'mita.file.fs:repository :root (get-thumbnail-root spec req)))

;; todo: directory traversal
(defun dir-add (spec req parent-dir files)
  (let ((dir-full-path (merge-pathnames parent-dir (get-content-root spec req))))
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
(defun dir-delete (spec req dir-path)
  (let ((dir-full-path (merge-pathnames dir-path (get-content-root spec req))))
    (uiop:delete-directory-tree dir-full-path
                                :validate t ;; todo
                                :if-does-not-exist :ignore)))


(defun dir-serve (spec req path &key on-folder on-file on-not-found)
  (when (not path)
    (return-from dir-serve (funcall on-not-found)))
  (let ((content-repos (content-repos spec req)))
    (let ((file (mita.file.fs:as-file content-repos path)))
      (cond ((not file)
             (funcall on-not-found))
            ((mita.file:folder-p file)
             (let ((files (mita.file:folder-list-children content-repos file)))
               (funcall on-folder file files)))
            (t
             (funcall on-file (mita.file.fs:file-full-path file)))))))

(defun dir-add-albums (spec req path)
  (let ((content-repos (content-repos spec req))
        (thumbnail-repos (thumbnail-repos spec req)))
    (let ((folders (mita.file.fs:list-folders content-repos path)))
      (when (not folders)
        (return-from dir-add-albums))
      (mita.db:with-connection (conn (get-db spec req))
        (mita.db:with-tx (conn)
          (mita.add-albums:run conn thumbnail-repos content-repos folders))))))
                               

;;;

(defun image-serve (spec req image-id-string
                    &key on-found
                         on-not-found)
  (mita.db:with-connection (conn (get-db spec req))
    (let ((image-id (mita.id:parse-short-or-nil image-id-string)))
      (unless image-id
        (return-from image-serve (funcall on-not-found)))
      (let ((image (mita.image:load-image conn image-id)))
        (unless image
          (return-from image-serve (funcall on-not-found)))
        (let ((root (cadr
                     (assoc
                      (mita.image:image-source image)
                      (list (list mita.image:+source-content+
                                  (get-content-root spec req))
                            (list mita.image:+source-thumbnail+
                                  (get-thumbnail-root spec req)))))))
          (unless root
            (return-from image-serve (funcall on-not-found)))
          (funcall on-found
                   (parse-namestring
                    (format nil "~A/~A"
                            root
                            (mita.image:image-path image)))))))))
