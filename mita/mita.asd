(asdf:defsystem :mita
  :serial t

  :components
  ((:file "timezone")

   (:file "id")
   (:file "image")
   (:file "album")
   (:file "album-with-images")
   (:file "tag")

   (:file "fs/fs")
   (:file "add-albums")

   (:file "thumbnail")
   (:file "fs/dir")

   (:file "db")
   (:file "tag-content")

   (:module :db-module
    :pathname "db"
    :components
    ((:module :rdb
      :pathname "rdb"
      :components
      ((:file "rdb")
       (:file "common")
       (:file "vendor/postgres")
       (:file "vendor/mysql")
       (:file "vendor/sqlite")
       (:file "image")
       (:file "album")
       (:file "tag")))

     (:file "file/file")

     (:file "impl")))
   (:file "account"))

  :depends-on
  (:mita-util-mysql

   :alexandria
   :cl-base64
   :cl-fad
   :cl-ppcre
   :local-time
   :postmodern
   :uuid
   :sqlite))
