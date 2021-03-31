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

   (:file "fs/fs")

   (:file "album-with-images")
   (:file "add-albums")

   (:file "thumbnail")
   (:file "fs/dir")

   (:file "db-postgres")
   (:file "db-file")
   (:file "db-impl")

   (:file "account"))

  :depends-on
  (:mita-util-postgres

   :alexandria
   :cl-base64
   :cl-fad
   :local-time
   :uuid

   ;; for db-file
   :cl-csv))

