(defpackage :mita.id
  (:use :cl)
  (:export :id
           :gen
           :gen-from-name
           :parse
           :parse-or-nil
           :parse-short-or-nil
           :to-string
           :to-string-short
           :id=))
(in-package :mita.id)

(defclass id (uuid:uuid) ())

(defun gen ()
  (change-class (uuid:make-v4-uuid) 'id))

(defun gen-from-name (key)
  (change-class (uuid:make-v5-uuid uuid:+namespace-dns+ key) 'id))

(defun parse (str)
  (change-class (uuid:make-uuid-from-string str) 'id))

(defun parse-or-nil (str)
  (handler-case (parse str)
    (error ()
      nil)))

(defun to-string (id)
  (format nil "~A" id))

(defun id= (id1 id2)
  (uuid:uuid= id1 id2))

(defun to-string-short (id)
  "Convert id into a short string used for URL and such."
  (let ((octets (uuid:uuid-to-byte-array id)))
    (let ((base64 (cl-base64:usb8-array-to-base64-string octets :uri t)))
      (subseq base64 0 22))))

(defun parse-short-or-nil (string)
  (handler-case
      (let ((octets (cl-base64:base64-string-to-usb8-array
                     (format nil "~A.." string) :uri t)))
        (let ((uuid (uuid:byte-array-to-uuid octets)))
          (change-class uuid 'id)))
    (error ()
      nil)))
