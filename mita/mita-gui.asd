(asdf:defsystem :mita-gui
  :serial t
  :pathname "src/gui"

  :components
  ((:file "state")
   (:file "tag-edit")
   (:file "album-list")
   (:file "album")
   (:file "view")
   (:file "ws/model")
   (:file "ws/jsown"))

  :depends-on
  (:mita
   :jsown
   :websocket-driver-server))
