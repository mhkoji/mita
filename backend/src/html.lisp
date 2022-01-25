(defpackage :mita.html
  (:use :cl)
  (:export :make-file
           :make-folder
           :folder
           :view
           :tags
           :not-found
           :internal-server-error))
(in-package :mita.html)

(defun folder (detail-json-string)
  (cl-who:with-html-output-to-string (s nil :prologue t)
    (:head
     (:meta :charset "utf-8")
     (:title "mita")
     (:link :rel "stylesheet"
            :href "/static/gen/folder.bundle.css"))
    (:body
     (cl-who:htm
      (:script :type "text/javascript"
       (cl-who:str (format nil "window['$d'] = ~A;" detail-json-string))))
     (:div :id "app")
     (:div :id "app-modal")
     (:script
      :type "text/javascript"
      :src "/static/gen/folder.bundle.js"))))

(defun view (viewer-json-string)
  (cl-who:with-html-output-to-string (s nil :prologue t)
    (:head
     (:meta :charset "utf-8")
     (:title "mita")
     (:link :rel "stylesheet"
            :href "/static/gen/view.bundle.css"))
    (:body
     (cl-who:htm
      (:script :type "text/javascript"
       (cl-who:str (format nil "window['$d'] = ~A;" viewer-json-string))))
     (:div :id "app")
     (:script
      :type "text/javascript"
      :src "/static/gen/view.bundle.js"))))

(defun tags ()
  (cl-who:with-html-output-to-string (s nil :prologue t)
    (:head
     (:meta :charset "utf-8")
     (:title "mita")
     (:link :rel "stylesheet"
            :href "/static/gen/tags.bundle.css"))
    (:body
     (:div :id "app")
     (:script
      :type "text/javascript"
      :src "/static/gen/tags.bundle.js"))))
