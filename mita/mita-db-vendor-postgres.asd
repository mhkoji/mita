(asdf:defsystem :mita-db-vendor-postgres
  :serial t

  :components
  ((:file "src/db/vendor/postgres"))

  :depends-on
  (:mita
   :postmodern))
