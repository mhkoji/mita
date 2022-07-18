(asdf:defsystem :mita
  :serial t
  :pathname "src/"
  :components
  ((:file "util/threading")
   (:file "file")
   (:file "tag"))
  :depends-on (:cl-csv
               :local-time
               :uuid))
