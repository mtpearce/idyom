(asdf:defsystem amuse-viewpoints
    :name "amuse-viewpoints"
    :depends-on ("amuse")
    :serial t
    :components ((:file "package")
                 (:file "utils")
                 (:file "generics")
                 (:file "classes")
                 (:file "methods")
                 (:file "functions")
                 (:file "macros")
                 (:file "viewpoint-definitions")))
