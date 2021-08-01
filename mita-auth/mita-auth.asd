(asdf:defsystem :mita-auth
  :serial t

  :components
  ((:module :admin
    :pathname "admin"
    :components
    ((:file "account")

     (:file "db/db")
     (:file "db/rdb/rdb")
     (:file "db/rdb/common")
     (:file "db/rdb/account")
     (:file "db/mysql")
     (:file "db/postgres")

     (:file "admin"))))

  :depends-on
  (:mita-util-password
   :mita-util-auth
   :mita
   :mita-db-vendor-mysql
   :mita-db-vendor-postgres))
