(defpackage :mita.web.ningle
  (:use :cl)
  (:export :start))
(in-package :mita.web.ningle)

(defvar *app*
  nil)

(defvar *root-path*
  nil)

(let ((app (make-instance 'ningle:app)))
  (setf (ningle:route app "/folder*")
        (lambda (params)
          (let ((namestring (cadr (assoc :splat params))))
            (let ((file (mita.file:os-make-file *root-path* namestring)))
              (if (mita.file:folder-p file)
                  (let ((folder-files (mita.file:folder-list-files
                                       file *root-path*)))
                    `(200 (:content-type "text/html")
                          (,(mita.web.html:folder
                             (mita.file:file-path file)
                             (mapcar
                              (lambda (f)
                                (mita.web.html:make-file
                                 :path (mita.file:file-path f)
                                 :full-path (mita.file:file-full-path f)))
                              (remove-if #'mita.file:folder-p folder-files))
                             (mapcar
                              (lambda (f)
                                (mita.web.html:make-folder
                                 :path (mita.file:file-path f)
                                 :thumbnail-file
                                 (let ((file (first
                                              (remove-if
                                               #'mita.file:folder-p
                                               (mita.file:folder-list-files
                                                f *root-path*)))))
                                   (when file
                                     (mita.web.html:make-file
                                      :path (mita.file:file-path file)
                                      :full-path
                                      (mita.file:file-full-path file))))))
                              (remove-if-not #'mita.file:folder-p
                                             folder-files))))))
                  `(200 ()
                        ,(parse-namestring
                          (mita.file:file-full-path file))))))))
  (setf (ningle:route app "/view*")
        (lambda (params)
          (let ((namestring (cadr (assoc :splat params))))
            (let ((file (mita.file:os-make-file *root-path* namestring)))
              `(200 (:content-type "text/html")
                    (,(mita.web.html:view
                       (when (mita.file:folder-p file)
                         (let ((folder-files (mita.file:folder-list-files
                                              file *root-path*)))
                           (mapcar
                            (lambda (f)
                              (mita.web.html:make-file
                               :path (mita.file:file-path f)
                               :full-path (mita.file:file-full-path f)))
                            (remove-if #'mita.file:folder-p
                                       folder-files)))))))))))
  (setf *app* app))

;;;

(defvar *static-root*
  (merge-pathnames
   "static/"
   (asdf:system-source-directory :mita-web)))

(defvar *handler*
  nil)

(defun start (&key (port 5000)
                   (use-thread t))
  (when *handler*
    (clack:stop *handler*))
  (setq *handler*
        (clack:clackup
         (lack:builder
          (:static :path "/static/" :root *static-root*)
          *app*)
         :address "0.0.0.0"
         :use-thread use-thread
         :port port)))
