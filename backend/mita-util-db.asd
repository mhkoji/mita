(asdf:defsystem :mita-util-db
  :serial t
  :pathname "src/util/db"
  :components
  ((:file "b-plus-tree"))
  :depends-on
  (:alexandria))
