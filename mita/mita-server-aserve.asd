(asdf:defsystem :mita-server-aserve
  :serial t
  :pathname "server/server/"

  :components
  ((:file "server")
   (:file "aserve"))

  :depends-on
  (:mita
   :mita-auth

   :aserve))
