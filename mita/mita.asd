(asdf:defsystem :mita
  :serial t
  :pathname "./"

  :components
  ((:file "id")
   (:file "db/db")
   (:file "mita")
   (:file "image")
   (:file "page")
   (:file "album")

   (:file "tag")
   (:file "tag-content")

   (:file "db/postgres")
   (:file "mita-postgres")

   (:file "dir")
   (:file "add-albums"))

  :depends-on
  (:alexandria
   :local-time
   :uuid
   :postmodern))
