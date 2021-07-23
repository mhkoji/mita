(asdf:defsystem :mita-gui-backend
  :serial t

  :components
  ((:file "state")
   (:file "gui")
   (:file "jsown")
   (:file "clack"))

  :depends-on
  (:mita
   :mita-web-backend

   :clack
   :websocket-driver-server
   :jsown))
