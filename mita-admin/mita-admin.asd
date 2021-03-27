(asdf:defsystem :mita-admin
  :serial t
  :pathname "src/"

  :components
  ((:module :account
    :pathname "account"
    :components ((:file "db")
                 (:file "account")
                 (:file "db-postgres")
                 (:file "db-file")))
   (:file "admin"))

  :depends-on
  (:mita
   :mita-util-password))
