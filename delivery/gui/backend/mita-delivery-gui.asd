(asdf:defsystem :mita-delivery-gui
  :serial t
  :pathname "src"

  :components
  ((:file "server")
   (:file "config"))

  :depends-on
  (:mita
   :mita-gui
   :mita-db-vendor-sqlite

   :clack))
