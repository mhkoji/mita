(asdf:defsystem :mita
  :serial t
  :pathname "server/"

  :components
  ((:file "timezone")

   (:file "id")
   (:file "db/db")
   (:file "image")
   (:file "album")

   (:file "tag")
   (:file "tag-content")

   (:file "fs/fs")

   (:file "album-with-images")
   (:file "add-albums")

   (:file "thumbnail")
   (:file "fs/dir")

   (:module :db
    :pathname "db"
    :components
    ((:file "relational")
     (:file "postgres")
     (:file "mysql")
     (:file "file")
     (:file "impl")))

   (:file "account"))

  :depends-on
  (:mita-util-mysql

   :alexandria
   :cl-base64
   :cl-fad
   :local-time
   :postmodern
   :uuid

   ;; for db-file
   :cl-csv))

