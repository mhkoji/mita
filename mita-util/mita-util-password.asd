(asdf:defsystem :mita-util-password
  :serial t
  :components
  ((:file "password"))
  :depends-on
  (:cl-bcrypt))
