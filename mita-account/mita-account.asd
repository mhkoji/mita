(asdf:defsystem :mita-account
  :serial t

  :components
  ((:file "db")
   (:file "account")

   (:file "postgres/postgres"))

  :depends-on
  (:mita
   :cl-bcrypt))
