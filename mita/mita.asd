(asdf:defsystem :mita
  :serial t
  :pathname "./"

  :components
  ((:file "id")
   (:file "db")
   (:file "mita")
   (:file "image")
   (:file "page")
   (:file "album")

   (:file "tag")
   (:file "tag-content")

   (:file "postgres/db")

   (:file "dir")
   (:file "add-albums")

   (:file "postgres/postgres"))

  :depends-on
  (:alexandria
   :local-time
   :uuid
   :postmodern

   ;; for account
   :cl-bcrypt))
