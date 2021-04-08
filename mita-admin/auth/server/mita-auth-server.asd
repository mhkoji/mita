(asdf:defsystem :mita-auth-server
  :serial t

  :components
  ((:module :admin
    :pathname "../../admin/server"
    :components
    ((:module :account
      :pathname "account"
      :components ((:file "db")
                   (:file "account")
                   (:file "db-postgres")
                   (:file "db-file")))
     (:file "admin")

     (:file "ningle")))

   (:file "ningle")
   (:file "server"))

  :depends-on
  (:mita-util-password
   :mita-util-auth
   :mita

   :ningle
   :cl-who
   :jsown
   :clack
   :lack
   :lack-middleware-session))
