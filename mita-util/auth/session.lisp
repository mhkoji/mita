(defpackage :mita.util.auth.session
  (:use :cl)
  (:import-from :lack.middleware.session.store
                :fetch-session
                :store-session
                :remove-session)
  (:export :redis-store
           :make-redis-store
           :fetch-session
           :store-session
           :remove-session)
  (:import-from :alexandria
                :when-let))
(in-package :mita.util.auth.session)

(defun serialize (data)
  (cl-base64:usb8-array-to-base64-string
   (trivial-utf-8:string-to-utf-8-bytes
    (prin1-to-string (marshal:marshal data)))))

(defun deserialize (data)
  (marshal:unmarshal
   (read-from-string
    (trivial-utf-8:utf-8-bytes-to-string
     (cl-base64:base64-string-to-usb8-array data)))))

(defstruct (redis-store (:include lack.middleware.session.store:store))
  (host "127.0.0.1")
  (port 6379)
  (namespace "session" :type string)
  (expires nil :type (or null integer)))

(defmacro with-connection (store &body body)
  `(redis:with-connection (:host (redis-store-host ,store)
                           :port (redis-store-port ,store))
     ,@body))

(defun key (store sid)
  (format nil "~A:~A" (redis-store-namespace store) sid))

(defmethod fetch-session ((store redis-store) sid)
  (when-let ((data (with-connection store
                     (red:get (key store sid)))))
    (handler-case (deserialize data)
      (error (e)
        (warn "Could not deserialize: ~A, ~A" data e)
        nil))))

(defmethod store-session ((store redis-store) sid session)
  (let ((key (key store sid))
        (data (serialize session)))
    (with-connection store
      (red:set key data)
      (when (redis-store-expires store)
        (red:expire key (redis-store-expires store))))))

(defmethod remove-session ((store redis-store) sid)
  (with-connection store
    (red:del (key store sid))))
