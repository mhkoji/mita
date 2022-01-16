(asdf:defsystem :mita
  :serial t
  :pathname "src"
  :components
  ((:file "file")
   (:file "tag"))
  :depends-on (:cl-csv
               :split-sequence
               :uuid))
