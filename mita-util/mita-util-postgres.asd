(asdf:defsystem :mita-util-postgres
  :serial t
  :components
  ((:file "postgres"))
  :depends-on
  (:postmodern))
