(asdf:defsystem :mita-gui-backend
  :serial t

  :components
  ((:file "state")
   (:file "tag-edit")
   (:file "album-list")
   (:file "album")
   (:file "view")
   (:file "ws/jsown")
   (:file "ws/processor")
   (:file "ws/clack"))

  :depends-on
  (:mita
   :mita-web-backend

   :clack
   :websocket-driver-server
   :jsown))
