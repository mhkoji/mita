(asdf:defsystem :mita-db-vendor-mysql
  :serial t

  :components
  ((:file "src/db/vendor/rdb")
   (:file "src/db/vendor/mysql"))

  :depends-on
  (:mita
   :mita-util-mysql))
