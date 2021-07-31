(defpackage :mita.web.app
  (:use :cl)
  (:export :spec
           :get-db
           :get-content-root
           :get-thumbnail-root
           :image-serve
           :album-upload
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
  
(defun album-upload (spec req files)
  (let ((full-paths nil)
        (content-root (get-content-root spec req)))
    (loop for (path stream) in files do
      (let ((full-path (concatenate 'string content-root path)))
        (ensure-directories-exist full-path)
        (with-open-file (out full-path
                             :direction :output
                             :element-type '(unsigned-byte 8)
                             :if-does-not-exist :create
                             :if-exists :overwrite)
          (alexandria:copy-stream stream out))
        (push full-path full-paths)))
    (let ((folder-paths (remove-duplicates
                         (mapcar (lambda (p)
                                   (namestring
                                    (cl-fad:pathname-directory-pathname p)))
                                 full-paths)
                         :test #'string=)))
      (let ((folders (mapcar (lambda (dp)
                            (mita.fs.dir:as-file content-root dp))
                          folder-paths))
            (thumbnail-folder (mita.fs.dir:as-file
                               (get-thumbnail-root spec req)
                               (get-thumbnail-root spec req))))
        (mita.db:with-connection (conn (get-db spec req))
          (mita.db:with-tx (conn)
            (mita.add-albums:run conn folders thumbnail-folder)))))))

;;;

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