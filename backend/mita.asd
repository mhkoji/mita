(asdf:defsystem :mita
  :serial t
  :pathname "src"
  :components
  ((:file "util/threading")
   (:file "file")
   (:file "tag")
   (:file "view")
   (:file "main"))
  :depends-on (:bordeaux-threads
               :cl-csv
               :split-sequence
               :uuid))
