(asdf:defsystem :mita-web-aserve
  :serial t
  :pathname "server/"

  :components
  ((:file "aserve"))

  :depends-on
  (:mita
   :mita-auth

   :aserve))
