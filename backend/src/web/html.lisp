(defpackage :mita.web.html
  (:use :cl)
  (:export :make-file
           :make-folder
           :folder
           :view
           :not-found
           :internal-server-error))
(in-package :mita.web.html)

(defstruct file
  path
  full-path)

(defstruct folder
  path
  thumbnail-file)

(defun file->jsown (file)
  (jsown:new-js
    ("path" (namestring (file-path file)))
    ("url"  (format nil "/folder/~A" (file-path file)))))

(defun folder->jsown (folder)
  (let ((path (folder-path folder)))
    (jsown:new-js
      ("path" (namestring path))
      ("url" (format nil "/folder/~A" path))
      ("thumbnail" (let ((file (folder-thumbnail-file folder)))
                     (if file (file->jsown file) :null))))))

(defun folder (path files folders)
  (cl-who:with-html-output-to-string (s nil :prologue t)
    (:head
     (:meta :charset "utf-8")
     (:title "mita")
     (:link :rel "stylesheet"
            :href "/static/gen/folder.bundle.css"))
    (:body
     (cl-who:htm
      (:script :type "text/javascript"
       (cl-who:str
        (format nil "window['$d'] = ~A;"
         (jsown:to-json
          (jsown:new-js
            ("path" (namestring path))
            ("files" (mapcar #'file->jsown files))
            ("folders" (mapcar #'folder->jsown folders))))))))
     (:div :id "app")
     (:div :id "app-modal")
     (:script
      :type "text/javascript"
      :src "/static/gen/folder.bundle.js"))))

(defun view (files)
  (cl-who:with-html-output-to-string (s nil :prologue t)
    (:head
     (:meta :charset "utf-8")
     (:title "mita")
     (:link :rel "stylesheet"
            :href "/static/gen/view.bundle.css"))
    (:body
     (cl-who:htm
      (:script :type "text/javascript"
       (cl-who:str
        (format nil "window['$d'] = ~A;"
         (jsown:to-json
          (jsown:new-js
            ("images" (mapcar #'file->jsown files))))))))
     (:div :id "app")
     (:script
      :type "text/javascript"
      :src "/static/gen/view.bundle.js"))))
