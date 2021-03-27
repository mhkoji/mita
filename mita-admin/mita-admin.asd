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
  (:mita-util-postgres
   :mita-util-password
   :mita))
