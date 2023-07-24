(asdf:defsystem :mita-web
  :pathname "src/"
  :serial t
  :components
  ((:file "web/folder")
   (:file "web/tag")
   (:file "web/service")
   (:file "json")
   (:file "html"))
  :depends-on (:mita
               :bordeaux-threads
               :cl-who
               :split-sequence
               :jsown))
