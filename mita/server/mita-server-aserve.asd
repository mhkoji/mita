(asdf:defsystem :mita-server-aserve
  :serial t
  :pathname "src/server"

  :components
  ((:file "app")
   (:file "aserve"))

  :depends-on
  (:mita
   :mita-util-auth
   :mita-auth-server

   :aserve))
