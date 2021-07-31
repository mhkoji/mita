(asdf:defsystem :mita-delivery-gui
  :serial t
  :pathname "src"

  :components
  ((:file "ws/jsown")
   (:file "ws/processor")
   (:file "ws/server"))

  :depends-on
  (:mita
   :mita-gui
   :mita-db-vendor-sqlite

   :clack
   :websocket-driver-server))
