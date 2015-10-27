(cl:in-package #:viewpoints)

;; Basic metrical viewpoints 

(define-basic-viewpoint isonset ((events md:grid-sequence))
  (md:isonset (last-element events)))

(define-basic-viewpoint pos ((events md:grid-sequence))
  (md:pos (last-element events)))
