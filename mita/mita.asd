(asdf:defsystem :mita
  :serial t
  :pathname "src"

  :components
  ((:file "timezone")

   (:file "id")
   (:file "image")
   (:file "album/album")
   (:file "album/list")
   (:file "album/with-images")
   (:file "tag")

   (:file "file/file")
   (:file "album/add")

   (:file "thumbnail")

   (:file "file/fs")

   (:file "db")
   (:file "tag-content")
   (:file "load-albums")

   (:module :db-module
    :pathname "db"
    :components
    ((:module :rdb
      :pathname "rdb"
      :components
      ((:file "rdb")
       (:file "image")
       (:file "album")
       (:file "tag")))

     (:file "file/file"))))

  :depends-on
  (:alexandria
   :cl-base64
   :cl-fad
   :cl-ppcre
   :local-time
   :uuid))
