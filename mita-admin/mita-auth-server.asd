(asdf:defsystem :mita-auth-server
  :serial t

  :components
  ((:module :src
    :pathname "src"
    :components
    ((:module :account
      :pathname "account"
      :components ((:file "db")
                   (:file "account")
                   (:file "db-postgres")
                   (:file "db-file")))
     (:file "admin")))

   (:module :admin/server
    :pathname "admin/server/"
    :components
    ((:file "ningle")))

   (:module :auth/server
    :pathname "auth/server/"
    :components
    ((:file "ningle")
     (:file "server"))))

  :depends-on
  (:mita-util-postgres
   :mita-util-password
   :mita-util-auth
   :mita

   :ningle
   :cl-who
   :jsown
   :clack
   :lack
   :lack-middleware-session))
