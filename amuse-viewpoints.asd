(asdf:defsystem amuse-viewpoints
    :name "amuse-viewpoints"
    :depends-on ("amuse" "amuse-mtp" "closer-mop")
    :serial t
    :components ((:file "package")
                 (:file "utils")
                 (:file "generics")
                 (:file "classes")
                 (:file "methods")
                 (:file "functions")
                 (:file "macros")
		 (:file "basic-viewpoints")
		 ;; Derived viewpoints
		 (:module derived-viewpoints
			  :components
			  ((:file "pitch")
			   (:file "temporal")
			   (:file "misc")
			   (:file "scales")
			   (:file "implication-realisation"))
)))

