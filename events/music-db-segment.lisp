

;;; Segmenters 
(defclass mtp-after-segmenter (ground-truth-segmenter after-segmenter mtp-object)
 ())

(defclass mtp-before-segmenter (ground-truth-segmenter before-segmenter 
                                                      mtp-object)
 ())

