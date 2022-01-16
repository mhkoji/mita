(asdf:defsystem :mita-util
  :serial t
  :pathname "src/util"
  :components
  ((:file "b-plus-tree"))
  :depends-on
  (:alexandria))
