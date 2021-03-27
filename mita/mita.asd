(asdf:defsystem :mita
  :serial t
  :pathname "server/"

  :components
  ((:file "timezone")

   (:file "id")
   (:file "db")
   (:file "image")
   (:file "page")
   (:file "album")

   (:file "tag")
   (:file "tag-content")

   (:file "dir")
   (:file "thumbnail")

   (:file "album-with-images")
   (:file "add-albums")

   (:file "postgres/db")
   (:file "postgres/postgres")

   (:file "db-file")
   (:file "db-impl"))

  :depends-on
  (:alexandria
   :local-time
   :uuid
   :postmodern
   :cl-fad

   ;; for db-file
   :cl-csv))

