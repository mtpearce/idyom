(asdf:defsystem ppm-star
  :name "ppm-star"
  :components 
  ((:file "ppm-star")
   (:file "ppm-io" :depends-on ("ppm-star"))
   (:file "daniel" :depends-on ("ppm-star"))))