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

   (:file "dir")
   (:file "add-albums")

   (:file "account/db")
   (:file "account/account")

   (:file "postgres/db")
   (:file "postgres/postgres"))

  :depends-on
  (:alexandria
   :local-time
   :uuid
   :postmodern
   :cl-fad

   ;; for account
   :cl-bcrypt))
