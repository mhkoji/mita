(defpackage :mita.web.html
  (:use :cl)
  (:export :*base-gen-path*
           :make-file
           :make-folder
           :folder
           :view
           :tags
           :not-found
           :internal-server-error))
(in-package :mita.web.html)

(defvar *base-gen-path*
  "/")

(defun gen-path (filename)
  (format nil "~A~A" *base-gen-path* filename))

(defun folder (detail-json-string)
  (cl-who:with-html-output-to-string (s nil :prologue t)
    (:head
     (:meta :charset "utf-8")
     (:title "mita")
     (:link :rel "stylesheet"
            :href (gen-path "folder.css")))
    (:body
     (cl-who:htm
      (:script :type "text/javascript"
       (cl-who:str (format nil "window['$d'] = ~A;" detail-json-string))))
     (:div :id "app")
     (:div :id "app-modal")
     (:script
      :type "text/javascript"
      :src  (gen-path "folder.js")))))

(defun view (viewer-json-string)
  (cl-who:with-html-output-to-string (s nil :prologue t)
    (:head
     (:meta :charset "utf-8")
     (:title "mita")
     (:link :rel "stylesheet"
            :href (gen-path "view.css"))
    (:body
     (cl-who:htm
      (:script :type "text/javascript"
       (cl-who:str (format nil "window['$d'] = ~A;" viewer-json-string))))
     (:div :id "app")
     (:script
      :type "text/javascript"
      :src (gen-path "view.js"))))))

(defun tags ()
  (cl-who:with-html-output-to-string (s nil :prologue t)
    (:head
     (:meta :charset "utf-8")
     (:title "mita")
     (:link :rel "stylesheet"
            :href (gen-path "tags.css"))
    (:body
     (:div :id "app")
     (:script
      :type "text/javascript"
      :src (gen-path "tags.js"))))))
