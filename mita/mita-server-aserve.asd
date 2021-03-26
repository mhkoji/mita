(asdf:defsystem :mita-server-aserve
  :serial t
  :pathname "server/server/"

  :components
  ((:file "app")
   (:file "aserve"))

  :depends-on
  (:mita
   :mita-util-auth
   :mita-auth-server

   :aserve))
