(asdf:defsystem :mita-web
  :pathname "src/"
  :serial t
  :components
  ((:file "view")
   (:file "web")
   (:file "html")
   (:file "json"))
  :depends-on (:mita
               :bordeaux-threads
               :cl-who
               :split-sequence
               :jsown))
