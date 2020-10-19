(defpackage :mita.web.server.html
  (:use :cl)
  (:export :pages
           :page
           :view
           :albums
           :album
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
      :src "/static/gen/pages.bundle.js"))))

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

(defun albums (gw offset limit)
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
                         ("id"
                          (mita.id:to-string (mita.album:album-id album)))
                         ("url"
                          (format nil "/albums/~A"
                                  (mita.id:to-string
                                   (mita.album:album-id album))))
                         ("name"
                          (mita.album:album-name album))
                         ("thumbnail"
                          (jsown:new-js
                            ("url"
                             (format nil "/images/~A"
                                     (mita.id:to-string
                                      (mita.image:image-id
                                       (mita.album:album-thumbnail
                                        album)))))))))
                     (mita.album:load-albums gw offset limit)))
            ("pager"
             (let ((format-str "/albums?offset=~A&limit=~A"))
               (jsown:new-js
                 ("prev"
                  (if (< 0 offset)
                      (format nil format-str
                              (max (- offset limit) 0) limit)
                      :null))
                 ("next"
                  (format nil format-str
                          (+ offset limit) limit)))))))))))
     (:div :id "app")
     (:div :id "app-modal")
     (:script
      :type "text/javascript"
      :src "/static/gen/albums.bundle.js"))));

(defun album (gw album)
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
               ("id"
                (mita.id:to-string (mita.album:album-id album)))
               ("name"
                (mita.album:album-name album))
               ("images"
                (mapcar (lambda (image)
                          (jsown:new-js
                            ("id"
                             (mita.id:to-string
                              (mita.image:image-id image)))
                            ("url"
                             (format nil "/images/~A"
                                     (mita.id:to-string
                                      (mita.image:image-id image))))))
                        (mita.album:album-images gw album)))))))))))
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
            ("images"
             (mapcar (lambda (image)
                       (jsown:new-js
                         ("id"
                          (mita.id:to-string
                           (mita.image:image-id image)))
                         ("url"
                          (format nil "/images/~A"
                                  (mita.id:to-string
                                   (mita.image:image-id image))))))
                     images))))))))
     (:div :id "app")
     (:script
      :type "text/javascript"
      :src "/static/gen/view.bundle.js"))))
