(asdf:defsystem :mita-delivery-web-full-mita-image
  :serial t
  :pathname "full/mita"

  :components
  ((:file "server-image"))

  :depends-on
  (:mita
   :mita-web
   :mita-util-auth
   :mita-delivery-web-full-auth
   :mita-delivery-web-full-mita

   :aserve))
