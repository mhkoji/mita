(defpackage :mita.web.html
  (:use :cl)
  (:export :make-file
           :make-folder
           :folder
           :view
           :not-found
           :internal-server-error))
(in-package :mita.web.html)

(defun folder (detail-jsown)
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
         (jsown:to-json detail-jsown)))))
     (:div :id "app")
     (:div :id "app-modal")
     (:script
      :type "text/javascript"
      :src "/static/gen/folder.bundle.js"))))

(defun view (file-jsown-list)
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
            ("images" file-jsown-list)))))))
     (:div :id "app")
     (:script
      :type "text/javascript"
      :src "/static/gen/view.bundle.js"))))
