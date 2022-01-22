(asdf:defsystem :mita
  :serial t
  :pathname "src"
  :components
  ((:file "file")
   (:file "tag"))
  :depends-on (:bordeaux-threads
               :cl-csv
               :split-sequence
               :uuid))
