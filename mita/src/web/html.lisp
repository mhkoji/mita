(defpackage :mita.web.html
  (:use :cl)
  (:export :view
           :home
           :albums
           :album
           :tags
           :dir
           :not-found
           :internal-server-error))
(in-package :mita.web.html)

(defun internal-server-error ()
  (cl-who:with-html-output-to-string (s nil :prologue t)
    (:head
     (:meta :charset "utf-8")
     (:title "mita"))
    (:body
     "Internal Server Error")))

(defun not-found ()
  (cl-who:with-html-output-to-string (s nil :prologue t)
    (:head
     (:meta :charset "utf-8")
     (:title "mita"))
    (:body
     "Not found")))

(defun albums (albums prev-url next-url)
  (cl-who:with-html-output-to-string (s nil :prologue t)
    (:head
     (:meta :charset "utf-8")
     (:title "mita")
     (:link :rel "stylesheet"
            :href "/static/gen/albums.bundle.css"))
    (:body
     (cl-who:htm
      (:script :type "text/javascript"
       (cl-who:str
        (format nil "window['$mita'] = ~A;"
         (jsown:to-json
          (jsown:new-js
            ("albums"
             (mapcar (lambda (album)
                       (jsown:new-js
                         ("id"  (mita.album:album-id album))
                         ("url" (mita.web.jsown:url-for album))
                         ("name" (mita.album:album-name album))
                         ("thumbnail" (or (mita.album:album-thumbnail album)
                                          :null))))
                     albums))
            ("pager"
             (jsown:new-js
               ("prev" (or prev-url :null))
               ("next" (or next-url :null))))))))))
     (:div :id "app")
     (:div :id "app-modal")
     (:script
      :type "text/javascript"
      :src "/static/gen/albums.bundle.js"))))

(defun album (album images)
  (cl-who:with-html-output-to-string (s nil :prologue t)
    (:head
     (:meta :charset "utf-8")
     (:title "mita")
     (:link :rel "stylesheet"
            :href "/static/gen/album.bundle.css"))
    (:body
     (cl-who:htm
      (:script :type "text/javascript"
       (cl-who:str
        (format nil "window['$mita'] = ~A;"
         (jsown:to-json
          (jsown:new-js
            ("album"
             (jsown:new-js
               ("id"     (mita.album:album-id album))
               ("name"   (mita.album:album-name album))
               ("images" images)))))))))
     (:div :id "app")
     (:div :id "app-modal")
     (:script
      :type "text/javascript"
      :src "/static/gen/album.bundle.js"))))

(defun view (images)
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
        (format nil "window['$mita'] = ~A;"
         (jsown:to-json
          (jsown:new-js
            ("images" (mapcar #'mita.web.jsown:as-image images))))))))
     (:div :id "app")
     (:script
      :type "text/javascript"
      :src "/static/gen/view.bundle.js"))))

(defun home ()
  (cl-who:with-html-output-to-string (s nil :prologue t)
    (:head
     (:meta :charset "utf-8")
     (:title "mita")
     (:link :rel "stylesheet"
            :href "/static/gen/home.bundle.css"))
    (:body
     (:div :id "app")
     (:script
      :type "text/javascript"
      :src "/static/gen/home.bundle.js"))))

(defun dir (folder files)
  (cl-who:with-html-output-to-string (s nil :prologue t)
    (:head
     (:meta :charset "utf-8")
     (:title "mita")
     (:link :rel "stylesheet"
            :href "/static/gen/dir.bundle.css"))
    (:body
     (cl-who:htm
      (:script :type "text/javascript"
       (cl-who:str
        (format nil "window['$mita'] = ~A;"
         (jsown:to-json
          (jsown:new-js
            ("path" (mita.file:file-path folder))
            ("files" files)))))))
     (:div :id "app")
     (:div :id "app-modal")
     (:script
      :type "text/javascript"
      :src "/static/gen/dir.bundle.js"))))


(defun tags (db)
  (cl-who:with-html-output-to-string (s nil :prologue t)
    (:head
     (:meta :charset "utf-8")
     (:title "mita")
     (:link :rel "stylesheet"
            :href "/static/gen/tags.bundle.css"))
    (:body
     (cl-who:htm
      (:script :type "text/javascript"
       (cl-who:str
        (format nil "window['$mita'] = ~A;"
         (jsown:to-json
          (jsown:new-js
            ("tags" (mita.tag:load-tags db))))))))
     (:div :id "app")
     (:script
      :type "text/javascript"
      :src "/static/gen/tags.bundle.js"))))
