(asdf:defsystem :mita
  :serial t

  :pathname "mita/"
  :components
  ((:file "id")
   (:file "db/db")
   (:file "mita")
   (:file "image")
   (:file "page")
   (:file "album")

   (:file "dir")
   (:file "add-albums")

   (:file "db/postgres")
   (:file "mita-postgres"))

  :depends-on (:alexandria
               :local-time
               :uuid
               :postmodern))
