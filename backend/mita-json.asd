(asdf:defsystem :mita-json
  :components
  ((:file "src/json"))
  :depends-on (:jsown
               :mita))
