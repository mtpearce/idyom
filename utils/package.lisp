;;;; ======================================================================
;;;; File:       package.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2003-04-05 18:54:17 marcusp>                           
;;;; Time-stamp: <2023-05-22 13:23:44 marcusp>                           
;;;; ======================================================================

(cl:in-package #:cl-user)

(defpackage #:utils
  (:use #:cl)
  (:export #:ask-user-y-n-question #:message
           #:initialise-progress-bar #:update-progress-bar
           #:dolist-pb #:dotimes-pb #:update-alist
           #:round-to-nearest-decimal-place #:approx-equal
           #:average #:generate-integers #:sd #:cor
           #:powerset #:quotient #:factorial #:n-permutations #:n-combinations
           #:sample #:parse-number 
           #:shuffle #:nth-root #:string-append #:split-string #:any-duplicated
           #:range #:cumsum #:md5-sum-of-list #:shuffle 
           #:nth-root #:any-duplicated
           #:insertion-sort #:cartesian-product #:flatten #:combinations
           #:flatten-order #:count-frequencies #:numeric-frequencies
           #:find-duplicates #:rotate #:permutations #:remove-by-position
           #:random-select
           #:list->string #:string-append #:split-string
           #:copy-instance  insert-after #:all-eql #:all-positions-if
           #:remove-nth #:csv->hash-table
           #:copy-slot-values #:initialise-unbound-slots
           #:last-element #:penultimate-element #:last-n #:butlast-n
           #:quantiles #:assign-to-quantile
           #:nmapcar #:nposition #:npositions #:nmember #:nmin #:nselectfirst
           #:alist->hash-table #:hash-table->alist #:hash-table->sorted-alist
           #:read-object-from-file #:file-exists #:write-object-to-file
           #:cd #:pwd #:ensure-directory #:copy-file #:recursively-list-files
           #:collect-garbage #:shell-command
           #:dataframe #:add-row #:bind-by-row #:remove-columns-except
           #:get-column #:sort-by-columns #:print-data
           #:as-dataframe #:write-csv
           #:data #:num-rows)
  (:documentation "Utility functions of general use."))

(defpackage #:python
  (:use #:cl)
  (:export #:alist->dict #:plist->dict #:list->list)
  (:documentation "Utility functions for exporting python code."))



