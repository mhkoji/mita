(asdf:defsystem :mita
  :serial t
  :pathname "src"

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

   (:module :rdb
    :pathname "rdb"
    :components
    ((:file "rdb")
     (:file "common")
     (:file "postgres")
     (:file "mysql")
     (:file "impl")
     (:file "image")
     (:file "album")
     (:file "tag")))

   (:file "tag-content")
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

