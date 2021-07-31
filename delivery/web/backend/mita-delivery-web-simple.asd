(asdf:defsystem :mita-delivery-web-simple
  :serial t
  :pathname "simple"

  :components
  ((:file "server"))

  :depends-on
  (:mita
   :mita-web
   :mita-db-vendor-sqlite
   :mita-util-clack
   :lack-middleware-static))
