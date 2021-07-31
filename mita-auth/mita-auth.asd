(asdf:defsystem :mita-auth
  :serial t

  :components
  ((:module :admin
    :pathname "admin"
    :components
    ((:file "db/db")
     (:file "db/mysql")
     (:file "db/postgres")

     (:module :account
      :pathname "account"
      :components ((:file "account")
                   (:file "db-file")
                   (:file "rdb")
                   (:file "rdb-account")
                   (:file "rdb-postgres")))
     (:file "admin"))))

  :depends-on
  (:mita-util-password
   :mita-util-auth
   :mita
   :mita-db-vendor-mysql
   :mita-db-vendor-postgres))
