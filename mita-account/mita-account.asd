(asdf:defsystem :mita-account
  :serial t
  :pathname "./"

  :components
  ((:file "db")
   (:file "account"))

  :depends-on
  (:mita
   :ironclad))
