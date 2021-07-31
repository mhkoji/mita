(asdf:defsystem :mita-delivery-web-full-mita
  :serial t
  :pathname "full/mita"

  :components
  ((:file "externs")
   (:file "server"))

  :depends-on
  (:mita
   :mita-web
   :mita-util-auth
   :mita-util-clack
   :mita-delivery-web-full-auth
   :lack-middleware-static
   :lack-middleware-session))
