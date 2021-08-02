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


(defun dir-serve (spec req path &key on-found on-not-found)
  (when (not path)
    (return-from dir-serve (funcall on-not-found)))
  (let* ((content-root (get-content-root spec req))
         (full-path (parse-namestring
                     (concatenate 'string content-root "/" path))))
    (when (not (cl-fad:file-exists-p full-path))
      (return-from dir-serve (funcall on-not-found)))
    (funcall on-found (mita.fs.dir:as-file content-root full-path))))

(defun dir-add-albums (spec req path)
  (let* ((content-root (get-content-root spec req))
         (full-path (parse-namestring
                     (concatenate 'string content-root "/" path))))
    (when (not (cl-fad:file-exists-p full-path))
      (return-from dir-add-albums))
    (let ((folders (mita.fs.dir:list-folders content-root full-path))
          (thumbnail-folder (mita.fs.dir:as-file
                             (get-thumbnail-root spec req)
                             (get-thumbnail-root spec req))))
      (mita.db:with-connection (conn (get-db spec req))
        (mita.db:with-tx (conn)
          (mita.add-albums:run conn folders thumbnail-folder))))))

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
