(asdf:defsystem ppm-star
  :name "ppm-star"
  :depends-on ("psgraph")
  :components 
  ((:file "package")
   (:file "generics" :depends-on ("package"))
   (:file "ppm-star" :depends-on ("package" "generics"))
   (:file "ppm-io" :depends-on ("package" "ppm-star"))
   (:file "ppm-ui" :depends-on ("package" "ppm-star"))))
