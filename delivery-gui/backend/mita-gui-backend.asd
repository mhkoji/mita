(asdf:defsystem :mita-gui-backend
  :serial t
  :pathname "src"

  :components
  ((:file "state")
   (:file "tag-edit")
   (:file "album-list")
   (:file "album")
   (:file "view")
   (:file "ws/jsown")
   (:file "ws/processor")
   (:file "ws/server"))

  :depends-on
  (:mita
   :mita-db-vendor-sqlite

   :clack
   :websocket-driver-server
   :jsown))
