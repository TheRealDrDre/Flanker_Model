(clear-all)

(define-model response-monkey

(sgp :trace-detail            high 
     :auto-attend             t)

;;; RITL chunks

(chunk-type (flanker-stimulus (:include visual-object))
	    kind nature left center right)

(chunk-type (flanker-screen (:include visual-object))
	    kind phase)

(chunk-type (flanker-stimulus-location (:include visual-location))
	    kind flanker)

(add-dm (I> isa chunk)
	(I< isa chunk)
	(stimulus isa chunk)
	(flanker-screen isa chunk)
	(flanker-stimulus isa chunk)
	(done isa chunk))

;;; Just encodes visual objects and reponds

(p encode-anything
   ?visual>
     state free
     buffer empty
   ?manual>
     preparation free
     processor free
     execution free
==>
  +visual-location>
     kind flanker-stimulus
     :attended nil
)

(p respond-to-anything
   =visual>
     kind flanker-stimulus
   ?visual>
     state free
   ?manual>
     preparation free
     processor free
     execution free
==>
   +manual>
     isa punch
     finger index
     hand left
)

)
