(asdf:defsystem :mita-web-backend-aserve
  :serial t

  :components
  ((:file "app")
   (:file "aserve"))

  :depends-on
  (:mita
   :mita-util-auth
   :mita-auth-backend

   :aserve))
