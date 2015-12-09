(cl:in-package #:viewpoints)

;; Basic metrical viewpoints 

(define-basic-viewpoint is-onset ((events md:grid-sequence))
  (md:is-onset (last-element events)))

(define-basic-viewpoint pos ((events md:grid-sequence))
  (md:pos (last-element events)))

(define-basic-viewpoint resolution ((events md:grid-sequence))
  (md:resolution (last-element events)))
