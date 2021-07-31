(defpackage :mita.delivery.web.full.auth.ningle
  (:use :cl)
  (:export :route-auth
           :route-admin))
(in-package :mita.delivery.web.full.auth.ningle)

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
                           (mita.auth.admin.account:account-id a)))
                         ("username"
                          (mita.auth.admin.account:account-username a))))
                     accounts)))))))
     (:div :id "app")
     (:div :id "app-modal")
     (:script
      :type "text/javascript"
      :src "/auth/static/gen/admin.bundle.js"))))

(defun route-admin (app db-manager
                    content-base
                    thumbnail-base)
  (setf (ningle:route app "/admin")
        (lambda (params)
          (declare (ignore params))
          (admin-page (mita.auth.admin:list-accounts db-manager))))
  (setf (ningle:route app "/admin/api/account" :method :post)
        (lambda (params)
          (declare (ignore params))
          (with-safe-json-response
            (let ((body
                   (lack.request:request-body-parameters ningle:*request*)))
              (mita.auth.admin:create-account
               db-manager
               (cdr (assoc "username" body :test #'string=))
               (cdr (assoc "password" body :test #'string=))
               content-base
               thumbnail-base))
            t)))
  (setf (ningle:route app "/admin/api/account/:id" :method :delete)
        (lambda (params)
          (with-safe-json-response
            (mita.auth.admin:delete-account
             db-manager
             (mita.id:parse (cdr (assoc :id params)))
             content-base
             thumbnail-base)
            t))))

;;;

(defclass account-repository ()
  ((db-manager :initarg :db-manager)))

(defmethod mita.util.auth:account-identity
    ((account mita.auth.admin.account:account))
  (mita.id:to-string (mita.auth.admin.account:account-id account)))

(defmethod mita.util.auth:find-account ((repos account-repository)
                                        username
                                        password)
  (when (and username password)
    (mita.db:with-connection (conn (mita.auth.admin:get-admin-db
                                    (slot-value repos 'db-manager)))
      (mita.auth.admin.account:find-account-with-password-checked
       conn username password))))

(defun login-page ()
  (cl-who:with-html-output-to-string (s nil :prologue t)
    (:head
     (:meta :charset "utf-8")
     (:title "mita")
     (:link :rel "stylesheet"
            :href "/auth/static/gen/login.bundle.css"))
    (:body
     (:div :id "app")
     (:script
      :type "text/javascript"
      :src "/auth/static/gen/login.bundle.js"))))

(defun route-auth (app db-manager &key top-url)
  (setf (ningle:route app "/auth/login")
        (lambda (params)
          (declare (ignore params))
          (if (mita.util.auth:is-authenticated-p
               (make-instance
                'mita.util.auth.lack:lack-session-holder
                :env (lack.request:request-env ningle:*request*)))
              `(302 (:location ,top-url) nil)
              (login-page))))

  (setf (ningle:route app "/auth/api/logout" :method :post)
        (lambda (params)
          (declare (ignore params))
          (with-safe-json-response
            (let ((env (lack.request:request-env ningle:*request*)))
              (mita.util.auth:logout
               (make-instance 'mita.util.auth.lack:lack-session-holder
                              :env env)))
            t)))

  (setf (ningle:route app "/auth/api/login" :method :post)
        (lambda (params)
          (declare (ignore params))
          (with-safe-json-response
            (let ((env
                   (lack.request:request-env ningle:*request*))
                  (body
                   (lack.request:request-body-parameters ningle:*request*)))
              (if (mita.util.auth:login
                   (make-instance 'mita.util.auth.lack:lack-session-holder
                                  :env env)
                   (make-instance 'account-repository
                                  :db-manager db-manager)
                   (cdr (assoc "username" body :test #'string=))
                   (cdr (assoc "password" body :test #'string=)))
                  t
                  :false))))))
