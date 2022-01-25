(asdf:defsystem :mita
  :serial t
  :pathname "src"
  :components
  ((:file "util/threading")
   (:file "file")
   (:file "tag")
   (:file "view")
   (:file "main")
   (:file "html"))
  :depends-on (:bordeaux-threads
               :cl-csv
               :cl-who
               :local-time
               :split-sequence
               :uuid))
