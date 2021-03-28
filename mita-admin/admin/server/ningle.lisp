(defpackage :mita.admin.server.ningle
  (:use :cl)
  (:export :route-admin))
(in-package :mita.admin.server.ningle)

(defun admin-page (accounts)
  (cl-who:with-html-output-to-string (s nil :prologue t)
    (:head
     (:meta :charset "utf-8")
     (:title "mita")
     (:link :rel "stylesheet"
            :href "/auth/static/gen/admin.bundle.css"))
    (:body
     (:script :type "text/javascript"
       (cl-who:str
        (format nil "window['$mita'] = ~A;"
         (jsown:to-json
          (jsown:new-js
            ("accounts"
             (mapcar (lambda (a)
                       (jsown:new-js
                         ("id"
                          (mita.id:to-string
                           (mita.admin.account:account-id a)))
                         ("username"
                          (mita.admin.account:account-username a))))
                     accounts)))))))
     (:div :id "app")
     (:script
      :type "text/javascript"
      :src "/auth/static/gen/admin.bundle.js"))))

(defun route-admin (app connector)
  (setf (ningle:route app "/admin")
        (lambda (params)
          (declare (ignore params))
          (admin-page (mita.admin:list-accounts connector)))))
