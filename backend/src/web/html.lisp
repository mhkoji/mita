(defpackage :mita.web.html
  (:use :cl)
  (:export :make-file
           :make-folder
           :folder
           :view
           :not-found
           :internal-server-error))
(in-package :mita.web.html)

(defun file->jsown (file)
  (jsown:new-js
    ("path" (namestring (mita.web.view:file-path file)))
    ("url"  (format nil "/folder/~A" (mita.web.view:file-path file)))))

(defun folder-overview->jsown (overview)
  (let ((path (mita.web.view:folder-overview-path overview)))
    (jsown:new-js
      ("path" (namestring path))
      ("url" (format nil "/folder/~A" path))
      ("thumbnail" (let ((file (mita.web.view:folder-overview-thumbnail-file
                                overview)))
                     (if file (file->jsown file) :null))))))

(defun folder (detail)
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
            ("path"
             (namestring (mita.web.view:folder-detail-path detail)))
            ("files"
             (mapcar #'file->jsown
                     (mita.web.view:folder-detail-file-list detail)))
            ("folders"
             (mapcar #'folder-overview->jsown
                     (mita.web.view:folder-detail-folder-overview-list
                      detail)))))))))
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
