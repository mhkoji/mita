(defpackage :mita.admin.server.ningle
  (:use :cl)
  (:export :route-admin))
(in-package :mita.admin.server.ningle)

(defmacro with-safe-json-response (&body body)
  `(let ((resp ningle:*response*))
     (alexandria:appendf (lack.response:response-headers resp)
                         (list :content-type "application/json"))
     (handler-case
         (jsown:to-json
          (jsown:new-js
            ("success" t)
            ("value" (progn ,@body))))
       (error (e)
         (warn "Error: ~A" e)
         (setf (lack.response:response-status ningle:*response*)
               500)
         (jsown:to-json
          (jsown:new-js
            ("success" :f)))))))

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
     (:div :id "app-modal")
     (:script
      :type "text/javascript"
      :src "/auth/static/gen/admin.bundle.js"))))

(defun route-admin (app locator postgres-dir
                    content-base
                    thumbnail-base)
  (setf (ningle:route app "/admin")
        (lambda (params)
          (declare (ignore params))
          (admin-page (mita.admin:list-accounts locator))))
  (setf (ningle:route app "/admin/api/account" :method :post)
        (lambda (params)
          (declare (ignore params))
          (with-safe-json-response
            (let ((body
                   (lack.request:request-body-parameters ningle:*request*)))
              (mita.admin:create-account
               (cdr (assoc "username" body :test #'string=))
               (cdr (assoc "password" body :test #'string=))
               locator
               postgres-dir
               content-base
               thumbnail-base))
            t)))
  (setf (ningle:route app "/admin/api/account/:id" :method :delete)
        (lambda (params)
          (with-safe-json-response
            (mita.admin:delete-account
             (mita.id:parse (cdr (assoc :id params)))
             locator
             content-base
             thumbnail-base)
            t))))
