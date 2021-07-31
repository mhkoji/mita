(asdf:defsystem :mita-delivery-web-simple
  :serial t
  :pathname "simple/src"

  :components
  ((:file "server")
   (:file "config"))

  :depends-on
  (:mita
   :mita-web
   :mita-db-vendor-sqlite
   :mita-util-clack
   :lack-middleware-static))
