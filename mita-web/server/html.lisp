(defpackage :mita.web.server.html
  (:use :cl)
  (:export :pages
           :page
           :albums
           :not-found
           :internal-server-error))
(in-package :mita.web.server.html)

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

(defun pages (pages)
  (cl-who:with-html-output-to-string (s nil :prologue t)
    (:head
     (:meta :charset "utf-8")
     (:title "mita"))
    (:body
     (cl-who:htm
      (:script :type "text/javascript"
       (cl-who:str
        (format nil "window['$mita'] = ~A;"
         (jsown:to-json
          (jsown:new-js
            ("pages"
             (mapcar (lambda (p)
                       (jsown:new-js
                         ("url"
                          (format nil "/pages/~A"
                                  (mita.id:to-string (mita.page:page-id p))))
                         ("name"
                          (mita.id:to-string (mita.page:page-id p)))))
                     pages))))))))

     (:div :id "app")
     ; Main Javascript must be loaded after the body was rendered.
     (:script
      :type "text/javascript"
      :src "/static/gen/pages.bundle.js")
     (:script
      :type "text/javascript"
      :src "https://code.jquery.com/jquery-2.2.2.min.js"
      :integrity "sha256-36cp2Co+/62rEAAYHLmRCPIych47CvdM+uTBJwSzWjI="
      :crossorigin "anonymous"))))

(defun page (gw page)
  (cl-who:with-html-output-to-string (s nil :prologue t)
    (:head
     (:meta :charset "utf-8")
     (:title "mita"))
    (:body
     (cl-who:htm
      (:script :type "text/javascript"
       (cl-who:str
        (format nil "window['$mita'] = ~A;"
         (jsown:to-json
          (jsown:new-js
            ("page"
             (jsown:new-js
               ("page-id"
                (mita.id:to-string (mita.page:page-id page)))
               ("text"
                (mita.page:page-text gw page))
               ("image-urls"
                (mapcar (lambda (image)
                          (format nil "/images/~A"
                                  (mita.id:to-string
                                   (mita.image:image-id image))))
                        (mita.page:page-images gw page)))))))))))
     (:div :id "app")
     (:script
      :type "text/javascript"
      :src "/static/gen/page.bundle.js")
     (:script
      :type "text/javascript"
      :src "https://code.jquery.com/jquery-2.2.2.min.js"
      :integrity "sha256-36cp2Co+/62rEAAYHLmRCPIych47CvdM+uTBJwSzWjI="
      :crossorigin "anonymous"))))


(defun albums (albums)
  (cl-who:with-html-output-to-string (s nil :prologue t)
    (:head
     (:meta :charset "utf-8")
     (:title "mita"))
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
                         ("id"
                          (mita.id:to-string (mita.album:album-id album)))
                         ("name"
                          (mita.album:album-name album))
                         ("thumbnailImage"
                          (jsown:new-js
                            ("url"
                             (format nil "/images/~A"
                                     (mita.id:to-string
                                      (mita.image:image-id
                                       (mita.album:album-thumbnail
                                        album)))))))))
                     albums))))))))
     (:div :id "app")
     (:script
      :type "text/javascript"
      :src "/static/gen/albums.bundle.js")
     (:script
      :type "text/javascript"
      :src "https://code.jquery.com/jquery-2.2.2.min.js"
      :integrity "sha256-36cp2Co+/62rEAAYHLmRCPIych47CvdM+uTBJwSzWjI="
      :crossorigin "anonymous"))))
