;;;; vectometry.asd

(asdf:defsystem #:vectometry
  :depends-on (#:vecto #:geometry)
  :serial t
  :components ((:file "package")
               (:file "vectometry")
               (:file "colors")))

