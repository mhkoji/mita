(asdf:defsystem :mita-db-vendor-sqlite
  :serial t

  :components
  ((:file "src/db/vendor/sqlite"))

  :depends-on
  (:mita
   :sqlite))
