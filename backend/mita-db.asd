(asdf:defsystem :mita-db
  :serial t
  :pathname "src/db"
  :components
  ((:file "b-plus-tree"))
  :depends-on
  (:alexandria))
