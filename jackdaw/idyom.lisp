;; IDyOM interface definition

(defmethod gm:model-sequence ((graph gm::feature-graph)
			      (events md:music-sequence)
			      &key construct? predict? writers)
  (dolist (writer writers)
    (gm::set-uid writer (md::uid events)))
  (gm:model-sequence graph (coerce events 'list)
		     :construct? construct? :predict? predict?
		     :writers writers))
